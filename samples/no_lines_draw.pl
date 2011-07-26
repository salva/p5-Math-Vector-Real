#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use GD;
# e Math::Vector::Real;

my $width = 1024;

sub scl { map $width * ($_ / 4 + .5), @_ }

my $im = GD::Image->new($width, $width);

my $white = $im->colorAllocate(255, 255, 255);
my $blue = $im->colorAllocate(100, 100, 255);
my $dark_red = $im->colorAllocate(200, 0, 0);
my $orange = $im->colorAllocate(255, 200, 0);

my @l = sort { substr($a, 0, 3) cmp substr($b, 0, 3) } <>;

my $num = qr/-?[\d\.eE\-]+/;
my $bad;

for (@l) {
    unless (/^(GOOD|BAD):\s*{($num),\s*($num)}(?:\s*{($num),\s*($num)}-{($num),\s*($num)})?$/o) {
        warn "unparsed: $_";
        next;
    }
    given($1) {
        when('GOOD') {
            $im->filledEllipse(scl($2, $3), 7, 7, $blue);
        }
        when('BAD') {
            # last if ++$bad > 20;
            # grep !defined $_, $4, $5, $6, $7 and say "$4-$5-$6-$7";
            # $im->filledEllipse(scl($4, $5), 7, 7, $blue);
            # $im->filledEllipse(scl($6, $7), 7, 7, $blue);
            $im->line(scl($2, $3, $4, $5), $dark_red);
            $im->line(scl($2, $3, $6, $7), $dark_red);
            $im->filledEllipse(scl($2, $3), 6, 6, $orange);
        }
    }
}

open my $fh, '>output.png';
print $fh $im->png;
close $fh;
