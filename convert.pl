#!/usr/bin/perl -l

use List::AllUtils qw(reduce min firstidx);
use Sub::Infix;

BEGIN {
  *ADD = infix { add(@_) };
  *MUL = infix { mul(@_) };
  *SUB = infix { dif(@_) };

  %replace = (
      "+" => "|ADD|",
      "-" => "|SUB|",
      "*" => "|MUL|", );
};

sub add { ($a, $b) = @_; ($a =~ /\d+/ ? "+" x $a : $a).("+" x $b) }
sub mul {
  $a = shift; $b = ("[>".("+" x shift)."<-]>");
  $a =~ /\d+/ ? ("+" x $a).$b : $a =~ />[^<]+$/ ?
  $a.(do {%r=(">"=>"<","<"=>">");($b=$b) =~ s/(<|>)/$r{$1}/g; $b}) : $a.$b
}
sub dif {
  ($a, $b) = @_;
  $a !~ /\d+/ ? $a.("-" x $b) : ($a-$b > 0 ? "+" : "-") x abs $a-$b
}

sub rep { ($eq = shift) =~ s/([\+\-\*])/$replace{$1}/g; $eq }

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

sub bf_from_code { eval rep min_eq shift }
sub bf_from_char { bf_from_code ord shift}

sub bf_print { shift."." }
