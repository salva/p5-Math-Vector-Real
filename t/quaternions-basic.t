#!/usr/bin/perl

use strict;

use Test::More tests => 82;

use Math::Trig;
use Math::Vector::Real;




my ($a,$b,$c,$d) = map {rand} 1..4;
my ($e,$f,$g,$h) = map {rand} 1..4;
my @randoms = map {rand} 1..4;

my $q1 = random_vector(4);
my $q2 = random_vector(4);
my $q3 = random_vector(4);

my $q = V(1,2,3,4);
is($q->[0], 1, "V builds the correct vector");
is($q->[1], 2, "V builds the correct vector");
is($q->[2], 3, "V builds the correct vector");
is($q->[3], 4, "V builds the correct vector");

my $p = $q->clone;
is($p->[0], 1, "clone builds the correct vector");
is($p->[1], 2, "clone builds the correct vector");
is($p->[2], 3, "clone builds the correct vector");
is($p->[3], 4, "clone builds the correct vector");

eq_scalar($p * $q, 1 + 4 + 9 + 16, "dot product");

eq_vector(~$q, [1, -2, -3, -4], "conjugate");

eq_vector($q, [1, 2, 3, 4], "conjugate leaves argument unchanged");

eq_vector(-$q, [-1, -2, -3, -4], "neg");

eq_vector($q + $p, [2, 4, 6, 8], "add");

eq_vector($q1 + $q2, $q2 + $q1, "add commutes");

eq_vector($q - $p, [0, 0, 0, 0], "sub");

eq_vector($q1 - $q2, - ($q2 - $q1), "sub anticommutes");

eq_vector($q1 - $q2, $q1 + (-$q2)), "sub is the same as adding the opposite");

eq_scalar($q->norm2, 1 + 4 + 9 + 16, "norm2");

eq_vector(3 * $p, [3, 6, 9, 12], "scalar multiplication");


my $q1c = ~$q1;
my $q1i = 1/$q1;

eq_vector($q1 x $q1c, [$q1->norm2, 0, 0, 0], "multiplying with a conjugate gives the squared norm");
eq_vector($q1 x $q1i, [1, 0, 0, 0], "multiplying with inverse gives unit");
eq_vector($q1 x ($q2 + $q3), $q1 x $q2 + $q1 x $q3,
          "quaternion multiplication is left-distributive with the addition");
eq_vector(($q1 + $q2) x $q3, $q1 x $q3 + $q2 x $q3,
          "quaternion multiplication is right-distributive with the addition");
eq_vector($q1 x ($q2 - $q3), $q1 x $q2 - $q1 x $q3,
          "quaternion multiplication is left-distributive with the subtraction");
eq_vector(($q1 - $q2) x $q3, $q1 x $q3 - $q2 x $q3,
          "quaternion multiplication is right-distributive with the subtraction");

eq_vector($q1 x ($q2 x $q3), ($q1 x $q2) x $q3,
          "quaternion multiplication is associative");

eq_vector($q x [1, 3, 7, 11], [ 70, 10, 0, 20], "mul");

eq_norm($q->versor, 1, "versor");

eq_vector(~$q->versor, 1/$q->versor, "the inverse of a unit quaternion is its conjugate");

my $nine = V(-9, 0, 0, 0);
my $nine_nine = $nine x $nine;
eq_vector($nine_nine, [81, 0, 0, 0], "real multiplied by itself");

my $exponent = 2 + int rand 98;
my $q1pow = $q1;
$q1exp x= $q1 for 2..$exponent;
SKIP: {
    skip "pow is not implemented", 4;
    eq_vector($q1**$exponent, $q1pow, "pow (int)");

    $exponent = rand 10;
    eq_norm($q1 ** $exponent, abs($q1) ** $exponent,
            "quaternion pow raise magnitude to same power");

    eq_vector($q1 ** -1, $q1i, "quaternion raised to (-1)th power is inverse");

    my $nine_2 = $nine ** 2;
    eq_vector($nine_nine, [81, 0, 0, 0], "real raised to the power of 2");
};


{
	my ($ux,$uy,$uz) = random_versor();
	my $x = rand;
	my $y = rand;
	my $z = rand;
	my $theta = rand;
	my $q = Math::Quaternion->new($x,$y*$ux,$y*$uy,$y*$uz);
	my $etox = exp($x);
	my $cy = cos($y);
	my $yc = $etox * sin($y);
	# exp(q) for q=(r,0,0,0) should give (exp(r),0,0,0)
	ok(checkquat(Math::Quaternion::exp(Math::Quaternion->new($z)),exp($z),0,0,0),
		"Exponentiation of real quaternion works");
	# exp(ut) for unit u, scalar t should give
	# ( cos(t), u*sin(t) )
	ok(checkquat(
	 Math::Quaternion::exp(Math::Quaternion->new($theta*$ux,$theta*$uy,$theta*$uz)),
	 cos($theta),$ux*sin($theta),$uy*sin($theta),$uz*sin($theta)),
	 	"Exponentiation of pure quaternion works");
		
	# exp(x+uy) = exp(x) ( cos(y) + u sin(y) )
	ok(checkquat( Math::Quaternion::exp($q), $etox*$cy,$yc*$ux,$yc*$uy,$yc*$uz),
		"Quaternion exponentiation exponentiates");


# Check logarithms

	ok(checkquat(Math::Quaternion::log(Math::Quaternion->new($x)),log($x),0,0,0),
		"Logarithm of real quaternion works");
	
	my $explogq = Math::Quaternion::exp(Math::Quaternion::log($q1));
	my $logexpq = Math::Quaternion::log(Math::Quaternion::exp($q1));

	ok(quatequal_fuzz($explogq,$logexpq),
		"Exponentiation and Logarithm are mutually inverse");
	ok(quatequal_fuzz(Math::Quaternion::log($q1),Math::Quaternion::log([$a0,$a1,$a2,$a3])),
		"Math::Quaternion::log([a,b,c,d]) works");
	
}

{
	my $ql = Math::Quaternion->new(-2,0,0,0);
	my $logq = Math::Quaternion::log($ql);
	ok( checkquat($logq,
		CORE::log(2),pi,0,0),
		"log( (-2,0,0,0) ) = (log(2),pi,0,0)");
}


# Check that exp(q) is the same as e**q

ok(quatequal_fuzz(
	Math::Quaternion::power(exp(1),$q1),
	Math::Quaternion::exp($q1)),
	"Scalar exp(1) raised to quaternion power is the same as exponentiation"
	);

	

my $s = rand; # Random scalar

ok(quatequal_fuzz(Math::Quaternion::power($s,$q2),
	Math::Quaternion::exp(Math::Quaternion::multiply($q2,log($s)))),
	"a**b == exp(b*log(a)) for scalar a, quaternion b");

ok(quatequal_fuzz(Math::Quaternion::power($q1,$s),
	Math::Quaternion::exp(Math::Quaternion::multiply($s,Math::Quaternion::log($q1)))),
	"a**b == exp(b*log(a)) for quaternion a, scalar b");

ok(quatequal_fuzz(Math::Quaternion::power($q1,$q2),
	Math::Quaternion::exp(Math::Quaternion::multiply($q2,Math::Quaternion::log($q1)))),
	"a**b == exp(b*log(a)) for quaternion a,b");

# Build a random unit vector in R^3.

my $av = random_versor();
my $bv = random_versor();

ok( equal_fuzz(1, abs($av)), "Sanity check: I can make a random unit vector properly");

my $theta = rand(0.25*$PI);
my $rotquat = undef;
my $rq2 = undef;

ok( $rotquat = Math::Quaternion::rotation($theta,$ax,$ay,$az),
	"Math::Quaternion::rotation does not fail");
ok( equal_fuzz($rotquat->rotation_angle , $theta),
	"rotation_angle works");
ok( $rq2 = Math::Quaternion->new({axis => [$ax,$ay,$az],angle=>$theta}),
	"Math::Quaternion->new(\\\%hash) does not fail");
ok( quatequal_fuzz($rotquat,$rq2),
	"Math::Quaternion->new(\\\%hash) produces correct rotation");

ok(!defined( eval { Math::Quaternion::rotation(23,"skidoo"); 1; }),
	"Math::Quaternion::rotation(rubbish) fails");
ok(!defined( eval { Math::Quaternion::rotation(23,"skidoo",3,4,5); 1; }),
	"Math::Quaternion::rotation(much rubbish) fails");

my @vec = ($ax,$ay,$az);
$rq2 = Math::Quaternion::rotation($theta,\@vec);
ok(quatequal_fuzz($rotquat,$rq2),"Math::Quaternion::rotation(\$theta,\\\@vec) works");
$rq2 = Math::Quaternion::rotation(\@vec,$theta);
ok(quatequal_fuzz($rotquat,$rq2),"Math::Quaternion::rotation(\\\@vec,\$theta) works");

my ($rax,$ray,$raz);
($rax,$ray,$raz) = $rotquat->rotation_axis;

ok( equal_fuzz($rax,$ax) && equal_fuzz($ray,$ay) && equal_fuzz($raz,$az),
	"rotation_axis works");
{
	my $qnull = Math::Quaternion->new(1,0,0,0);
	my ($zx,$zy,$zz) = $qnull->rotation_axis;
	ok(equal_fuzz(0,$zx) && equal_fuzz(0,$zy) && equal_fuzz($zz,1),
		"(1,0,0,0)->rotation_axis() returns Z axis rather than crashing");

}

my ($x,$y,$z) = map {rand} 1..3;
my @r=$rotquat->rotate_vector($x,$y,$z);
ok( 3==@r, "Math::Quaternion::rotate_vector() produces a 3-vector");

my ($xx,$yy,$zz) = @r;
my $m1 = sqrt($x*$x+$y*$y+$z*$z);
my $m2 = sqrt($xx*$xx + $yy*$yy + $zz*$zz);

ok( equal_fuzz($m1,$m2), "Rotation preserves length");

# Let v and a be vectors. The component of v parallel to a is
# |v| cos theta = (v.a)/|a|, or (v.a) ahat, in vector form.
# Hence, the component of v perp to a is v - (v.a) ahat ; if
# a is a unit vector, then it's just v - (v.a) a .
# Let v' be v rotated by angle phi about a. Let
# u' and u be the components of v' and v perpendicular to a.
# Then u.u' == |u| |u'| cos phi == |u|^2 cos phi

my $vdota = $x*$ax + $y*$ay + $z*$az;
my ($ux,$uy,$uz) =    ($x -$vdota*$ax, $y -$vdota*$ay, $z -$vdota*$az);
my ($uxx,$uyy,$uzz) = ($xx-$vdota*$ax, $yy-$vdota*$ay, $zz-$vdota*$az);

my $dotproduct = $ux*$uxx + $uy*$uyy + $uz*$uzz;
my $modu = sqrt($ux*$ux+$uy*$uy+$uz*$uz);
my $moduu = sqrt($uxx*$uxx+$uyy*$uyy+$uzz*$uzz);

ok( equal_fuzz($modu,$moduu),
"Rotation preserves component perpendicular to axes");

ok( equal_fuzz($dotproduct,$modu*$moduu*cos($theta)),
	"Rotated vector makes correct dot product with original");



my @m;

ok( @m = $rotquat->matrix4x4, "matrix4x4() doesn't fail...");
ok( 16==@m, "...and produces a 4x4 matrix...");

my $mx = $m[ 0]*$x + $m[ 4]*$y + $m[ 8]*$z;
my $my = $m[ 1]*$x + $m[ 5]*$y + $m[ 9]*$z;
my $mz = $m[ 2]*$x + $m[ 6]*$y + $m[10]*$z;

ok( equal_fuzz($mx,$xx) && equal_fuzz($my,$yy) && equal_fuzz($mz,$zz),
	"...which produces the same rotation as the quaternion.");

my ($mr,$mir) = $rotquat->matrix4x4andinverse;
my @mr = @$mr; my @mir = @$mir;

$mx = $mr[ 0]*$x + $mr[ 4]*$y + $mr[ 8]*$z;
$my = $mr[ 1]*$x + $mr[ 5]*$y + $mr[ 9]*$z;
$mz = $mr[ 2]*$x + $mr[ 6]*$y + $mr[10]*$z;

ok( equal_fuzz($mx,$xx) && equal_fuzz($my,$yy) && equal_fuzz($mz,$zz),
	"matrix4x4andinverse() produces the same rotation matrix...");

my $mmx = $mir[ 0]*$mx + $mir[ 4]*$my + $mir[ 8]*$mz;
my $mmy = $mir[ 1]*$mx + $mir[ 5]*$my + $mir[ 9]*$mz;
my $mmz = $mir[ 2]*$mx + $mir[ 6]*$my + $mir[10]*$mz;

ok( equal_fuzz($mmx,$x) && equal_fuzz($mmy,$y) && equal_fuzz($mmz,$z),
	"...as well as its inverse.");

ok( @m = $rotquat->matrix3x3, "matrix3x3() doesn't fail...");
ok( 9==@m, "...and produces a 3x3 matrix...");

$mx = $m[ 0]*$x + $m[ 3]*$y + $m[ 6]*$z;
$my = $m[ 1]*$x + $m[ 4]*$y + $m[ 7]*$z;
$mz = $m[ 2]*$x + $m[ 5]*$y + $m[ 8]*$z;

ok( equal_fuzz($mx,$xx) && equal_fuzz($my,$yy) && equal_fuzz($mz,$zz),
	"...which produces the same rotation as the quaternion.");

my @v1=($ax,$ay,$az);
my @v2=($bx,$by,$bz);
ok($rotquat = Math::Quaternion::rotation(\@v1,\@v2),
	"Math::Quaternion::rotation(\\\@v1,\\\@v2) does not fail");

ok($rq2 = Math::Quaternion->new({ 'v1'=>\@v1, 'v2'=>\@v2 }),
	"Math::Quaternion->new({v1=>..,v2=>..}) does not fail");
ok(quatequal_fuzz($rotquat,$rq2),
	"Math::Quaternion->new({v1=>..,v2=>..}) produces correct quaternion");
ok(!defined( eval { Math::Quaternion->new({badger=>1,wibble=>7}); 1; }),
	"Math::Quaternion->new(rubbish) fails");

my ($cx,$cy,$cz) = $rotquat->rotation_axis;
ok(
	equal_fuzz(0,$ax*$cx+$ay*$cy+$az*$cz) &&
	equal_fuzz(0,$bx*$cx+$by*$cy+$bz*$cz) ,
	"Rotation axis perpendicular to start and end orientations");

my $dotprod = $ax*$bx+$ay*$by+$az*$bz;
my $moda = sqrt($ax*$ax+$ay*$ay+$az*$az);
my $modb = sqrt($bx*$bx+$by*$by+$bz*$bz);
ok( equal_fuzz($dotprod,$moda*$modb*cos($rotquat->rotation_angle)),
	"Rotation angle is angle between start and end vectors");

my ($dx,$dy,$dz) = $rotquat->rotate_vector($ax,$ay,$az);
ok( 
	equal_fuzz($bx,$dx) && equal_fuzz($by,$dy) && equal_fuzz($bz,$dz),
	"Rotation maps start vector onto end vector"
);

my $squat = Math::Quaternion->new(0,1,2,3);
ok( Math::Quaternion::stringify($squat) eq "( 0 1 2 3 )",
	"Stringification works");


{
 my @axis = (0,0,1);
 my $rq1 = Math::Quaternion::rotation(pi/2,\@axis);   # 90  degrees about Z
 my $rq2 = Math::Quaternion::rotation(pi,\@axis);     # 180 degrees about Z

 my $interp = Math::Quaternion::slerp($rq1,$rq2,0.5); # 135 degrees about Z
 my ($ax,$ay,$az) = $interp->rotation_axis;
 ok( equal_fuzz(0,$ax) && equal_fuzz(0,$ay) && equal_fuzz(1,$az),
 	"Math::Quaternion::slerp() produces correct rotation axis");
 ok( equal_fuzz(0.75*pi,$interp->rotation_angle),
 	"Math::Quaternion::slerp() produces correct rotation angle");

 $rq2 = Math::Quaternion::rotation((pi/2)+1e-6,\@axis);
 $interp = Math::Quaternion::slerp($rq1,$rq2,0.75);
 ok( quatequal_fuzz($interp,
 	Math::Quaternion::plus(
		Math::Quaternion::scale($rq1,0.25),
		Math::Quaternion::scale($rq2,0.75)
	)),
	"Math::Quaternion::slerp works linearly for small angles");

  $rq1 = Math::Quaternion->new({ axis=>[0,0,1],angle=>0.25*pi});
  $rq2 = Math::Quaternion->new({ axis=>[0,0,1],angle=>1.5*pi});
  $interp = Math::Quaternion::slerp($rq1,$rq2,0.75);
  ($ax,$ay,$az) = $interp->rotation_axis;
 ok( equal_fuzz(0,$ax) && equal_fuzz(0,$ay) && equal_fuzz(-1,$az),
 	"Math::Quaternion::slerp() gives correct axis for -ve dp");
 ok( equal_fuzz(0.3125*pi,$interp->rotation_angle),
 	"Math::Quaternion::slerp() gives correct angle for -ve dp");

}
my $uq = Math::Quaternion->new({v1 => [ 1,1,1],
                       v2 => [ 2,2,2],
                     });
ok( checkquat($uq,1,0,0,0),
    "Creating Quaternion from two parallel vectors does not crash");
