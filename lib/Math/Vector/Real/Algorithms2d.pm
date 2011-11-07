package Math::Vector::Real::Algorithms2d;

use strict;
use warnings;

use Math::Vector::Real;
use Sort::Key::Radix qw(nkeysort);

use 5.010;

require Exporter;
our @ISA = qw(Exporter);

our @EXPORT_OK = qw(convex_hull);

sub convex_hull {
    if (@_ < 2) {
        return map V(@$_), @_;
    }
    else {
        my @p = nkeysort { $_->[0] } @_;
        my @r;
        for my $dir (1, -1) {
            my @c;
            my $i = 0;
            while ($i < @p) {
                my ($x, $y) = @{$p[$i]};
                my $j = $i + 1;
                while ($j < @p and $p[$j][0] == $x) {
                    my $y1 = $p[$j][1];
                    $y = $y1 if ($y1 <=> $y) == $dir;
                    $j++;
                }
                my $v = V($x, $y);
                while (1) {
                    if (@c < 2) {
                        push @c, $v;
                        last;
                    }
                    else {
                        my $v0 = $c[-1] - $c[-2];
                        my $v1 = $v     - $c[-2];
                        my $dy_est = $v0->[1] / $v0->[0] * $v1->[0];
                        # print "estimated dy: $dy_est, dy: $v1->[1], dir: $dir\n";
                        # if (($dy_est <=> $v1->[1]) == $dir) {
                        if ($v0->[1] * $v1->[0] > $v1->[1] * $v0->[0]) {
                            push @c, $v;
                            last
                        }
                        # print "removing point\n";
                        pop @c;
                    }
                }
                $i = $j;
            }
            if ($dir == 1) {
                @r = @c;
                @p = reverse @p;
            }
            else {
                pop @r if ($c[0][1] == $r[-1][1]);
                push @r, @c;
                pop @r if @r > 1 and $r[0][1] == $r[-1][1];
            }
        }
        return @r;
    }
}

1;
