#!/usr/bin/perl
use warnings;
use strict;

open(DATA, "<example.txt");

while (<DATA>) {
	print "$_";
}
