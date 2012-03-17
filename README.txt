Robotocalypseopolis
=============================

Visit http://dev.i.tv:3000/




Installing robolis
------------------

To run on a local development server, install GHC 7.2 or later. See server/setup for instructions on Ubuntu 11.10. `runghc Main` will start the server. 

To install on windows: google "download ghc"

To work on client: just hit the live api: http://dev.i.tv:3000/

NEXT STEPS
[x] Cleanup bot (remove heartbeat)
[ ] MCP Layer
[ ] shared descriptions?
[ ] bulk processing?
[ ] actions instead of /move
[ ] Bug: unit can move while it is being destroyed, which makes it stay in the world, but not exist

PERFORMANCE
[ ] GZip
[ ] Bulk Commands? (easier if we switch to /units/:unitId/act) with an action command, except each one needs a different body :(

TODO

[x] deploy doesn't use same version of redis ()
[x] unitauth delete user
[ ] document delete user
[ ] docs on github pages (test cross-domain, as well)
[ ] actual heartbeat / cleanup

[x] Chaos Bot - Movement 
[x] Able to launch multiple bots from the viewer
[ ] Auto cleanup (on deploy, figure out easier way to start stuff)
[x] Bot type, but name
[x] Bot color
[x] Special indicator around YOUR bot (a border)
[ ] Deploy
[ ] Convince Bryce to build one. Play with a cool objective

[x] Get it compiling
[-] test to see if you can use raise/rescue instead of case statements /fault (auto fault with error)
[x] functional tests
[x] send back a starting location
[x] safe spawn
[x] block edge of the world movement 
[x] middleware validate user auth token (or cleaner way to do it)
[x] deploy
[ ] test movement (move, spawn loc, edge)
[x] unit cleanup (heartbeat?)
[ ] blocks - place blocks near you
[ ] collect - collect blocks

[ ] description - home page / repository
[ ] description - robot description fields

[x] world viewer
[ ] world viewer - circles and squares. colors. 
[ ] sample bot - just moves around
[ ] player - a way to play by hand

SUPPORT
[ ] documentation
[ ] website & writeup

BIG CHANGES
[ ] Game timer. You can only say "left", or "select" and it happens on the next tick if you haven't changed it. 

UNIT CLEANUP
[x] units:id:heartbeat, scan every single member of units, check if heartbeat is too old
[ ] units:heartbeats:DATE - add them as a member to the right one. Expire these sets. SDIFF them against units

GAME OF LIFE
[x] create a unit at a point
[-] shared types: all bryce's bots will be the same type and have the same description? No, they could have different colors. Just keep it the same. 
[x] destroy own unit
[x] add color to description

[x] make the canvas viewer better
[x] make a bot that moves around

?????
[ ] return more information / updates with the world call?
[ ] make our world hash more flexible. it can return a Unit, a BlockType, or whatever. 
[ ] have them specify the control password for a given unit

FAQ

Q. Why not use sockets?
A. Sockets would make updates, and automatic cleanup easier (built in heartbeat), but they would make scaling harder later. HTTP is inherently easy to scale/cache etc. 




