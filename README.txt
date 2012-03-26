Robotocalypseopolis
=============================

Visit http://dev.i.tv:3000/


so you'd have per player (maybe set up when starting the game) a TVar (Maybe Command)
10:14 PM
once you've got a request, atomically (writeTVar myTVar mycommand)
10:14 PM
in the request handler, run in whatever thread the server feels like
10:15 PM
then when the game loop wants to proces it does something like atomically (sequence [swapTVar player Nothing | player <- players)
10:15 PM seanhess
super cool
10:15 PM nexx has left IRC (Ping timeout: 265 seconds)
10:15 PM napping
which will get the orders, zero them out, and not have any risk of erasing an order if a new one arrives at the wrong time
10:16 PM
sequence [atomically (swapTVar player Nothing) | player <- players]
10:16 PM seanhess
I'll go play with it. Thanks so much for your help!

when you make an action, does it automatically set you to stop afterwards? (YES)
    - how can you make sure you're not losing information?
    - you need a big atomic operation: find and update

    - or do the easier thing and articifically rate-limit people
    !! Do that. It's WAY easier

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



SIMPLER IDEAS FOR MOVEMENT
- send down everything?
- prepare for differential updates

- minions - contains all minions
- unit:1:location (stores the location)

1. get the world
2. make sure a location is available
3. delete
4. move


CURRENT
state: hgetall
open: hsetnx
delete: remove location, remove unit info (PROBLEM: if moving)
move: hsetnx, update location

SINGLE INDEX?
state: units + mget :location
open: ??? who knows? you need an index of :location
delete: remove from units
move: update location

DOUBLE INDEX
locations:10:3 = unitId -- tells us whether its occupied
units:5:location
units - the active units -- make it so if you delete them there is NO WAY they can show up, even if you try to update their location later. 

-- I don't know that it would be any slower. 
O(2N) vs O(N)

-- getting the world state is one of the most elementary operations. 
-- I should wait to decide this until I get differential updates in place
-- because before that it's a waste of time. 

SO, i should just add a check to see if you're still in units before settting the location

-- why keep track of unit location?

THE PROBLEM:
    is that units can set their location after they have been cleared. 

I should err on the other side, wait until they've "settled"???

