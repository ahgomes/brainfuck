#!/usr/bin/perl -l

use List::AllUtils qw(reduce min indexes firstidx);
use Sub::Infix;

BEGIN {
  *ADD = infix { ($a, $b) = @_; ($a =~ /\d+/ ? "+" x $a : $a).("+" x $b) };
  *MUL = infix { ("+" x shift) . ("[>" . ("+" x shift) . "<-]>") }
};

%replace = (
    "+" => "|ADD|",
    "*" => "|MUL|",
);

sub cnv {
  @r = (shift, ""); $b = shift;
  until ($r[0] == 0) { @r = ( -1 & $r[0] / $b, ($r[0] % $b) . $r[1] ) }
  return $r[1];
}

sub bf_eq {
  $n = shift; $base = shift;
  $eq = reduce {"($a*$base+$b)"} split //, $n;
  $eq =~ s/(\+0)|(1\*)//g;
  $eq = substr($eq, 1, -1);
  if ($eq =~ s/(\(.*\))//g) { $eq = (eval $1).$eq }
  return $eq;
}

sub eq_len { $eq = shift =~ s/(\*)/\+6\+/r; return eval $eq }

sub min_eq {
  $x = shift; @b = 4 .. 17;
  @eqs = map {(bf_eq((cnv $x, $_), $_))} @b;
  @eq_lens = map {eq_len $_} @eqs;
  return @eqs [firstidx {$_ == min @eq_lens} @eq_lens];
}

sub bf_code {
  $eq = min_eq shift; $eq =~ s/([+]|[*])/$replace{$1}/g;
  return (eval $eq).".[-]";
}

foreach $code (map { ord } split //, <STDIN>) {
  print bf_code $code;
}
