#!/usr/bin/perl -l

use List::AllUtils qw(reduce min firstidx);
use Sub::Infix;
use subs qw( add mul dif rep
             cnv bf_eq eq_len min_eq neg_eq
             bf_print bf_clear bf_shift
             bf_from_code bf_from_char bf_from_string );

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
  $a = shift; $b = shift;
  $b = ("[>".(($b > 0 ? "+" : "-") x abs $b)."<-]>");
  $a =~ /\d+/ ? ("+" x $a).$b : $a =~ />[^<]+$/ ?
  $a.(do {%r=(">"=>"<","<"=>">");($b=$b) =~ s/(<|>)/$r{$1}/g; $b}) : $a.$b
}
sub dif {
  ($a, $b) = @_;
  $a !~ /\d+/ ? $a.("-" x $b) : ($a-$b > 0 ? "+" : "-") x abs $a-$b
}

sub rep { ($eq = shift) =~ s/((?<=\d)[\+\-\*])/$replace{$1}/g; $eq }

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
  return "$x+0" if $x < 5;
  @eqs = map {(bf_eq((cnv $x, $_), $_))} @b;
  @eq_lens = map {eq_len $_} @eqs;
  return @eqs [firstidx {$_ == min @eq_lens} @eq_lens];
}

sub neg_eq {
  if (($eq = shift) =~ s/(\*)/$&-/g) {
    return $eq if ($eq =~ s/(\+)/-/g);
  }
  return "0-".(eval $eq);
}

sub bf_print { shift."." }
sub bf_clear { shift."[-]" }
sub bf_shift {
  ($n = shift) > 0 ? bf_from_code($n) : eval rep neg_eq min_eq abs $n
}

sub bf_from_code { eval rep min_eq shift }
sub bf_from_char { bf_from_code ord shift}
sub bf_from_string {
  ($prev, @rest) = map { ord } split //, shift;
  $result = bf_print bf_from_code $prev;
  foreach $curr (@rest) {
    $r = bf_print bf_shift($curr-$prev);
    $result .= ($r =~ /\[/ ? "<" : "").$r;
    $prev = $curr;
  }
  return $result;
}

print bf_from_string "Hello, world!\n";
