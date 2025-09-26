---
title: "Ocean Sprint: Nix under palm trees"
tags: [nix, ocean-sprint, sprint, hackathon, nixos]
date: 2022-11-30
language: en
---

I've just attended my first sprint, that is a kind of long hackathon, in this
case lasting 5 days and exclusively Nix themed.

I had the occasion to meet a variety of people with different attitudes to
problems and different backgrounds, this last thing was really interesting for
me since I think that in the last months I've lived inside a bubble made of
exclusively Haskellers.

![Some Nixers on the summit of a nearby volcano](/images/ocean-sprint-volcano-nixers.jpg)
*Some Nixers on the summit of a nearby volcano*

Personally I've decided to devote most of my time on Nix internals since it's
something that could help with the creation of new "2nix" tools (in the last
months I had many headaches due to tools like [haskell.nix](https://github.com/input-output-hk/haskell.nix) and their extensive
use of IFDs). My colleague [Márton](https://github.com/brainrake) and me also had a call with [John Ericson](https://github.com/Ericson2314) which
let to [this PR](https://github.com/NixOS/nix/pull/7339).

I've also spent a day digging into [Hydra](git@github.com:NixOS/hydra.git) trying to make it show the evaluation
log during the evaluation itself and not only after that the evaluation
terminated. In the end I wasn't able to make it work (it was more difficult than
I initially thought) but it was an useful experience and perhaps in the future
I'll continue these efforts.

Other people worked on different projects like making secure boot work on NixOS,
on nixifing a live coding environment, on [this](https://github.com/OceanSprint/tesh) really interesting project about
making shell snippets in markdown testable, on [nil](https://github.com/oxalica/nil) (a really powerful LSP server
for Nix written in Rust) or also [noogle](https://github.com/hsjobeki/noogle), a [Hoogle](https://hoogle.haskell.org/) like engine for Nix.