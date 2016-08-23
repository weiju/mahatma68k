# Mahatma68k is a Motorola 68000 emulator library written in Java and Ruby.

## Introduction

"Mahatma" means "Great Soul", which fits well to the fact that the 68000
used to be power some of the greatest computers of all time: e.g.
Amiga, Atari ST, Apple Macintosh, SGI IRIS, NeXT... These all were
considered dream machines in the mid-eighties, before the rise of the
PC as we know it today.

I have to admit it - I always loved 680x0 powered computers, they had
personality and soul, something I miss in today's machines, which
are more or less pretty much the same.

Mahatma68k is dedicated to all people who love computing, who are interested
in computing history and a time when Personal Computing was in its
infancy.

I associate the happiest time with computers with the 68000 CPU and
through this project, I hope to give a little bit of that back to the
community.

- Wei-ju Wu, September 9, 2009 (9/9/09)

## Building

It is assumed that you have Ruby and Maven 2 installed on your machine.
There is no maven plugin to call the generator and it will therefore
generate the single Cpu.java file into the source tree in order for the
Java compiler to find it.

./generator.rb
mvn install

See the example in the examples directory to see the general usage.
