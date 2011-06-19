package Math::Vector::Real;

our $VERSION = '0.06';

use strict;
use warnings;
use Carp;
use POSIX ();

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

sub new {
    my $class = shift;
    bless [@_], $class
}

sub new_ref {
    my $class = shift;
    bless [@{shift()}], $class;
}

sub zero {
    my ($class, $dim) = @_;
    $dim >= 0 or croak "negative dimension";
    bless [(0) x $dim], $class
}

sub is_zero { grep $_, @$_[0] }

sub cube {
    my ($class, $dim, $size) = @_;
    bless [($size) x $dim], $class;
}

sub axis_versor {
    my ($class, $dim, $ix);
    if (ref $_[0]) {
        my ($self, $ix) = @_;
        $class = ref $self;
        $dim = @$self;
    }
    else {
        ($class, $dim, $ix) = @_;
        $dim >= 0 or croak "negative dimension";
    }
    ($ix >= 0 and $ix < $dim) or croak "axis index out of range";
    my $self = [(0) x $dim];
    $self->[$ix] = 1;
    bless $self, $class
}

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

sub set {
    &_check_dim;
    my ($v0, $v1) = @_;
    $v0->[$_] = $v1->[$_] for 0..$#$v1;
}

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

sub abs2 {
    my $acu = 0;
    $acu += $_ * $_ for @{$_[0]};
    $acu;
}

sub dist {
    &_check_dim;
    my ($v0, $v1) = @_;
    my $d2 = 0;
    for (0..$#$v0) {
	my $d = $v0->[$_] - $v1->[$_];
	$d2 += $d * $d;
    }
    sqrt($d2);
}

sub dist2 {
    &_check_dim;
    my ($v0, $v1) = @_;
    my $d2 = 0;
    for (0..$#$v0) {
	my $d = $v0->[$_] - $v1->[$_];
	$d2 += $d * $d;
    }
    $d2;
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

sub versor {
    my $self = shift;
    my $f = 0;
    $f += $_ * $_ for @$self;
    $f == 0 and croak "Illegal division by zero";
    $f = 1/sqrt $f;
    bless [map $f * $_, @$self]
}

sub wrap {
    my ($self, $v) = @_;
    &_check_dim;
    require POSIX;

    bless [map  { my $s = $self->[$_];
		  my $c = $v->[$_];
		  $c - $s * POSIX::floor($c/$s) } (0..$#$self)];
}

sub max {
    my $max = 0;
    for (@{shift()}) {
	my $abs = CORE::abs($_);
	$abs > $max and $max = $abs;
    }
    $max
}

sub min {
    my $self = shift; 
    my $min = CORE::abs($self->[0]);
    for (@$self) {
	my $abs = CORE::abs($_);
	$abs < $min and $min = $abs;
    }
    $min
}

sub box {
    shift;
    return unless @_;
    my $min = shift->clone;
    my $max = $min->clone;
    my $dim = @$min - 1;
    for (@_) {
        for my $ix (0..$dim) {
            my $c = $_->[$ix];
            if ($max->[$ix] < $c) {
                $max->[$ix] = $c;
            }
            elsif ($min->[$ix] > $c) {
                $min->[$ix] = $c
            }
        }
    }
    ($min, $max);
}

sub max_component_index {
    my $self = shift;
    return unless @$self;
    my $max = $self->[0];
    my $max_ix = 0;
    for my $ix (1..$#$self) {
        if ($self->[$ix] > $max) {
            $max_ix = $ix;
            $max = $self->[$ix];
        }
    }
    $max_ix;
}

sub decompose {
    my ($u, $v) = @_;
    my $p = $u * ($u * $v)/abs2($u);
    my $n = $v - $p;
    wantarray ? ($p, $n) : $n;
}

sub canonical_base {
    my ($class, $dim) = @_;
    my @base = map { bless [(0) x $dim], $class } 1..$dim;
    $base[$_] = 1 for 0..$#base;
    return @base;
}

sub normal_base {
    my $v = shift;
    my $dim = @$v;
    if ($dim == 2) {
        my $u = $v->versor;
        @$u = ($u->[1], -$u->[0]);
        return $u;
    }
    else {
        my @base = Math::Vector::Real->canonical_base($dim);
        $_ = $v->decompose($_) for @base;
        for my $i (0..$dim - 2) {
            my $max = abs2($base[$i]);
            if ($max < 0.3) {
                for my $j ($i+1..$#base) {
                    my $d2 = abs2($base[$j]);
                    if ($d2 > $max) {
                        @base[$i, $j] = @base[$j, $i];
                        last unless $d2 < 0.3;
                        $max = $d2;
                    }
                }
            }
            my $versor = $base[$i] = $i->versor;
            $_ = $versor->decompose($_) for @base[$i+1..$#base];
        }
        pop @base;
        wantarray ? @base : $base[0];
    }
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

=head2 Extra methods

Besides the common mathematical operations described above, the
following methods are available from the package.

Note that all these methods are non destructive returning new objects
with the result.

=over 4

=item $v = Math::Vector::Real->new(@components)

Equivalent to C<V(@components)>.

=item $zero = Math::Vector::Real->zero($dim)

Returns the zero vector of the given dimension.

=item $v = Math::Vector::Real->cube($dim, $size)

Returns a vector of the given dimension with all its components set to
C<$size>.

=item $u = Math::Vector::Real->axis_versor($dim, $ix)

Returns a unitary vector of the given dimension parallel to the axis
with index C<$ix> (0-based).

For instance:

  Math::Vector::Real->axis_versor(5, 3); # V(0, 0, 0, 1, 0)
  Math::Vector::Real->axis_versor(2, 0); # V(1, 0)

=item @b = Math::Vector::Real->canonical_base($dim)

Returns the canonical base for the vector space of the given
dimension.

=item $u = $v->versor

Returns the versor for the given vector.

=item $wrapped = $w->wrap($v)

Returns the result of wrapping the given vector in the box defined by
C<$w>.

=item $max = $v->max

Returns the maximum of the absolute values of the vector components.

=item $min = $v->min

Returns the minimum of the absolute values of the vector components.

=item $d2 = $b->abs2

Returns the norm of the vector squared.

=item $d = $v->dist($u)

Returns the distance between the two vectors.

=item $d = $v->dist2($u)

Returns the distance between the two vectors squared.

=item ($bottom, $top) = Math::Vector::Real->box($v0, $v1, $v2, ...)

Returns the two corners of a hyper-box containing all the given
vectors.

=item $d = $v->set($u)

Equivalent to C<$v = $u> but without allocating a new object.

Note that this method is destructive.

=item $d = $v->max_component_index

Return the index of the vector component with the maximum size.

=item ($p, $n) = $v->decompose($u)

Decompose the given vector C<$u> in two vectors: one parallel to C<$v>
and another normal.

In scalar context returns the normal vector.

=item @b = $v->normal_base

Returns a set of vectors forming an ortonormal base for the hyperplane
normal to $v.

In scalar context returns just some random normal vector.

=back

=head1 SEE ALSO

L<Math::Vector::Real::Random> extends this module with random vector
generation methods.

L<Math::GSL::Vector>, L<PDL>.

There are other vector manipulation packages in CPAN (L<Math::Vec>,
L<Math::VectorReal>, L<Math::Vector>), but they can only handle 3
dimensional vectors.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009, 2011 by Salvador FandiE<ntilde>o
(sfandino@yahoo.com)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.0 or,
at your option, any later version of Perl 5 you may have available.

=cut
