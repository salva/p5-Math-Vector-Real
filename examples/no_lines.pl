#!/usr/bin/perl

# Generate unaligned random points in 2D space
#
# See: http://stackoverflow.com/q/6825772/124951

use strict;
use warnings;
use 5.010;

use Math::Vector::Real;
use Math::Vector::Real::Random;
use Sort::Key::Radix qw(nkeysort);

use constant PI => 3.14159265358979323846264338327950288419716939937510;

@ARGV <= 2 or die "Usage:\n  $0 [n_points [tolerance]]\n\n";

my $n_points = shift // 4000;
my $tolerance = shift // 0.01;

$tolerance = $tolerance * PI / 180;
my $tolerance_arctan = 3 / 2 * $tolerance;
my $tolerance_sin2 = sin($tolerance) ** 2;

sub cross2d {
    my ($p0, $p1) = @_;
    $p0->[0] * $p1->[1] - $p1->[0] * $p0->[1];
}

sub line_p {
    my ($p0, $p1, $p2) = @_;
    my $a0 = $p0->abs2 || return 1;
    my $a1 = $p1->abs2 || return 1;
    my $a2 = $p2->abs2 || return 1;
    my $cr01 = cross2d($p0, $p1);
    my $cr12 = cross2d($p1, $p2);
    my $cr20 = cross2d($p2, $p0);
    $cr01 * $cr01 / ($a0 * $a1) < $tolerance_sin2 or return;
    $cr12 * $cr12 / ($a1 * $a2) < $tolerance_sin2 or return;
    $cr20 * $cr20 / ($a2 * $a0) < $tolerance_sin2 or return;
    return 1;
}

my ($c, $f1, $f2, $f3) = (0, 1, 1, 1);

my @p;
GEN: for (1..$n_points) {
    my $q = Math::Vector::Real->random_normal(2);
    $c++;
    $f1 += @p;
    my @B = map {
        my ($dx, $dy) = @{$_ - $q};
        abs($dy) > abs($dx) ? $dx / $dy : $dy / $dx;
    } @p;

    my @six = nkeysort { $B[$_] } 0..$#B;

    for my $i (0..$#six) {
        my $B0 = $B[$six[$i]];
        my $pi = $p[$six[$i]];
        for my $j ($i + 1..$#six) {
            last if $B[$six[$j]] - $B0 > $tolerance_arctan;
            $f2++;
            my $pj = $p[$six[$j]];
            if (line_p($q - $pi, $q - $pj, $pi - $pj)) {
                $f3++;
                say "BAD: $q $pi-$pj";
                redo GEN;
            }
        }
    }
    push @p, $q;
    say "GOOD: $q";
    my $good = @p;
    my $ratiogood = $good/$c;
    my $ratio12 = $f2/$f1;
    my $ratio23 = $f3/$f2;
    print STDERR "gen: $c, good: $good, good/gen: $ratiogood, f2/f1: $ratio12, f3/f2: $ratio23                                  \r";
}
print STDERR "\n";
