package Math::Vector::Real;

our $VERSION = '0.02';

use strict;
use warnings;
use Carp;

use Exporter qw(import);
our @EXPORT = qw(V);

our %op = (add => '+',
	   neg => 'neg',
	   sub => '-',
	   mul => '*',
	   div => '/',
	   cross => 'x',
	   add_me => '+=',
	   sub_me => '-=',
	   mul_me => '*=',
	   div_me => '/=',
	   abs => 'abs',
	   atan2 => 'atan2',
	   equal => '==',
	   nequal => '!=',
	   clone => '=',
	   as_string => '""');

our %ol;
$ol{$op{$_}} = \&{${Math::Vector::Real::}{$_}} for keys %op;

require overload;
overload->import(%ol);

sub V { bless [@_] }

sub new { shift; bless [@_] }

sub _caller_op {
    my $level = (shift||1) + 1;
    my $sub = (caller $level)[3];
    $sub =~ s/.*:://;
    my $op = $op{$sub};
    (defined $op ? $op : $sub);
}

sub _check_dim {
    eval { @{$_[0]} == @{$_[1]} } and return;
    my $op = _caller_op(1);
    my $loc = ($_[2] ? 'first' : 'second');
    UNIVERSAL::isa($_[1], 'ARRAY') or croak "$loc argument to vector operator '$op' is not a vector";
    croak "vector dimensions do not match";
}

sub clone { bless [@{$_[0]}] }

sub add {
    &_check_dim;
    my ($v0, $v1) = @_;
    bless [map $v0->[$_] + $v1->[$_], 0..$#$v0]
}

sub add_me {
    &_check_dim;
    my ($v0, $v1) = @_;
    $v0->[$_] += $v1->[$_] for 0..$#$v0;
    $v0;
}

sub neg { bless [map -$_, @{$_[0]}] }

sub sub {
    &_check_dim;
    my ($v0, $v1) = ($_[2] ? @_[1, 0] : @_);
    bless [map $v0->[$_] - $v1->[$_], 0..$#$v0]
}

sub sub_me {
    &_check_dim;
    my ($v0, $v1) = @_;
    $v0->[$_] -= $v1->[$_] for 0..$#$v0;
    $v0;
}

sub mul {
    if (ref $_[1]) {
	&_check_dim;
	my ($v0, $v1) = @_;
	my $acu = 0;
	$acu += $v0->[$_] * $v1->[$_] for 0..$#$v0;
	$acu;
    }
    else {
	my ($v, $s) = @_;
	bless [map $s * $_, @$v];
    }
}

sub mul_me {
    if (ref $_[1]) {
	&_check_dim;
	my ($v0, $v1) = @_;
	my $acu = 0;
	$acu += $v0->[$_] * $v1->[$_] for 0..$#$v0;
	$acu;
    }
    else {
	my ($v, $s) = @_;
	$_ *= $s for @$v;
	$v
    }
}

sub div {
    croak "can't use vector as dividend"
	if ($_[2] or ref $_[1]);
    my ($v, $div) = @_;
    $div == 0 and croak "illegal division by zero";
    my $i = 1/$div;
    bless [map $i * $_, @$v]
}

sub div_me {
    croak "can't use vector as dividend" if ref $_[1];
    my $v = shift;
    my $i = 1 / shift;
    $_ *= $i for @$v;
    $v;
}

sub equal {
    &_check_dim;
    my ($v0, $v1) = @_;
    $v0->[$_] == $v1->[$_] || return 0 for 0..$#$v0;
    1;
}

sub nequal {
    &_check_dim;
    my ($v0, $v1) = @_;
    $v0->[$_] == $v1->[$_] || return 1 for 0..$#$v0;
    0;
}

sub cross {
    &_check_dim;
    my ($v0, $v1) = ($_[2] ? @_[1, 0] : @_);
    my $dim = @$v0;
    if ($dim == 3) {
	return bless [$v0->[1] * $v1->[2] - $v0->[2] * $v1->[1],
		      $v0->[2] * $v1->[0] - $v0->[0] * $v1->[2],
		      $v0->[0] * $v1->[1] - $v0->[1] * $v1->[0]]
    }
    if ($dim == 7) {
	croak "cross product for dimension 7 not implemented yet, patches welcome!";
    }
    else {
	croak "cross product not defined for dimension $dim"
    }
}

sub as_string { "{" . join(", ", @{$_[0]}). "}" }

sub abs {
    my $acu = 0;
    $acu += $_ * $_ for @{$_[0]};
    sqrt $acu;
}

sub _upgrade {
    my $dim;
    map {
	my $d = eval { @{$_} };
	defined $d or croak "argument is not a vector or array";
	if (defined $dim) {
	    $d == $dim or croak "dimensions do not match";
	}
	else {
	    $dim = $d;
	}
	UNIVERSAL::isa($_, __PACKAGE__) ? $_ : clone($_);
	    
    } @_;
}

sub atan2 {
    my ($v0, $v1) = @_;
    my $a0 = &abs($v0);
    return 0 unless $a0;
    my $u0 = $v0 / $a0;
    my $p = $v1 * $u0;
    atan2(&abs($v1 - $p * $u0), $p);
}

1;
__END__

=head1 NAME

Math::Vector::Real - Real vector arithmetic in Perl

=head1 SYNOPSIS

  use Math::Vector::Real;

  my $v = V(1.1, 2.0, 3.1, -4.0, -12.0);
  my $u = V(2.0, 0.0, 0.0,  1.0,   0.3);

  printf "abs(%s) = %d\n", $v, abs($b);
  my $dot = $u * $v;
  my $sub = $u - $v;
  # etc...

=head1 DESCRIPTION

A simple pure perl module to manipulate vectors of any dimension.

The function C<V>, always exported by the module, allows to create new
vectors:

  my $v = V(0, 1, 3, -1);

Vectors are represented as blessed array references. It is allowed to
manipulate the arrays directly as far as only real numbers are
inserted (well, actually, integers are also allowed because from a
mathematical point of view, integers are a subset of the real
numbers).

Example:

  my $v = V(0.0, 1.0);

  # extending the 2D vector to 3D:
  push @$v, 0.0;

  # setting some component value:
  $v->[0] = 23;

Vectors can be used in mathematical expressions:

  my $u = V(3, 3, 0);
  $p = $u * $v;       # dot product
  $f = 1.4 * $u + $v; # scalar product and vector addition
  $c = $u x $v;       # cross product, only defined for 3D vectors
  # etc.

The currently supported operations are:

  + * /
  - (both unary and binary)
  x (cross product for 3D vectors)
  += -= *= /= x=
  == !=
  "" (stringfication)
  abs (returns the norm)
  atan2 (returns the angle between two vectors)

That, AFAIK, are all the operations that can be applied to vectors.

When an array reference is used in an operation involving a vector, it
is automatically upgraded to a vector. For instance:

  my $v = V(1, 2);
  $v += [0, 2];

=head1 SEE ALSO

L<Math::GSL::Vector>, L<PDL>.

There are other vector manipulation packages in CPAN (L<Math::Vec>,
L<Math::VectorReal>, L<Math::Vector>), but they can only handle 3
dimensional vectors.

=head1 AUTHOR

Salvador Fandiño, E<lt>sfandino@Eyahoo.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Salvador Fandiño

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.0 or,
at your option, any later version of Perl 5 you may have available.

=cut
