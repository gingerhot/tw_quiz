#!/usr/bin/env perl
# B1nj0y  idegorepl<at>gmail.com

use v5.10;

my @data = (
  '5 5',
  '1 2 N',
  'LMLMLMLMM',
  '3 3 E',
  'MMRMMRMRRM',
  '4 4 N',
  'MMM'
);

my @directions = qw/W N E S/;
my $lower_left = [ 0, 0 ];
my $upper_right = get_coordinate();

while (@data) {
  my $position = get_coordinate();
  my @instructions = get_instruction();
  foreach my $instruction (@instructions) {
    my $where_am_i = turn_now($position, $instruction);
    $position = judge($where_am_i);
    last unless $position;
  }
  say join " ", @{$position} if $position;
}


sub get_coordinate {
  [ split / /, &get_line ];
}

sub get_instruction {
  split //, &get_line;
}

sub turn_now {
  my ($position, $instruction) = @_;

  if ($instruction eq 'M') {
    return next_position($position);
  }
  else {
    my $direction = next_direction($position->[2], $instruction);
    return [$position->[0], $position->[1], $direction];
  }
}

sub next_position {
  my $position = shift;
  if ($position->[2] eq 'N') {
    [$position->[0], $position->[1] + 1, $position->[2]];
  }
  elsif ($position->[2] eq 'S') {
    [$position->[0], $position->[1] - 1, $position->[2]];
  }
  elsif ($position->[2] eq 'W') {
    [$position->[0] - 1, $position->[1], $position->[2]];
  }
  else {
    [$position->[0] + 1, $position->[1], $position->[2]];
  }
}

sub next_direction {
  my ($current_direction, $instruction) = @_;
  my $index = get_index($current_direction);
  my $val = $instruction eq 'L' ? -- $index : ++ $index;
  $val < @directions ? $directions[$val] : $directions[0];
}

sub get_line {
  shift @data;
}

sub get_index {
  my $val = shift;
  for (0..$#directions) {
    return $_ if $directions[$_] eq $val;
  }
}

sub judge {
  my $position = shift;
  if ( $position->[0] >= $lower_left->[0] and $position->[0] <= $upper_right->[0] and
    $position->[1] >= $lower_left->[1] and $position->[1] <= $upper_right->[1] ) {
    return $position;
  }
  else {
    say "RIP" and return;
  }
}
