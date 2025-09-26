---
title: SCRML (SCheMeRogueLike)
tags: [scheme, roguelike]
date: 2017-09-11
language: en
---

Welcome to SCMRL (SCheMeRogueLike, yes I've to choose a more imaginative name),
in this page I will collect the information about this my project.

**2017/09/11** SCMRL is [here on GitHub](https://github.com/andrea96/scmrl). This is the first commit, I worked on it at
the end of July and I decided to upload it only now. The game is not playable
but I feared that it could be accidentally deleted so here it is.

**May 2019** After nearly 2 years of nothing I started working again on this
project, I'm trying to create an object oriented interface (using [coops](http://wiki.call-cc.org/eggref/5/coops)) for the
already existing structures. I'm also making it compatible with [Chicken 5](http://wiki.call-cc.org/man/5/). I
gave up about the idea of using the bugged [nCurses](http://wiki.call-cc.org/eggref/5/ncurses) egg and chose to directly do
the C calls to the library. At the moment I'm working on a new branch of the
repository.

Features implemented:

- Random dungeon generation inspired by [this](http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/)
- Field of vision based on [this method](http://www.roguebasin.com/index.php?title=Precise_Shadowcasting_in_JavaScript)
- Pathfinding via Dijkstra maps

I hope, sooner or later, to reach a playable version.