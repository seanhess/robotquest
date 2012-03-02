Botland
=============================

developmentNTqx5Vr75

TODO

[x] Get it compiling
[-] test to see if you can use raise/rescue instead of case statements /fault (auto fault with error)
[x] functional tests
[x] send back a starting location
[x] safe spawn
[x] block edge of the world movement 
[x] middleware validate user auth token (or cleaner way to do it)
[ ] deploy
[ ] test movement (move, spawn loc, edge)
[ ] unit cleanup (heartbeat?)

[ ] description - home page / repository
[ ] description - robot description fields

[ ] build a world viewer!
[ ] build a bot!

SUPPORT
[ ] website
[ ] documentation

BIG CHANGE
[ ] Game timer. You can only say "left", or "select" and it happens on the next tick if you haven't changed it. 

UNIT CLEANUP
1. could just leave them there util someone kills them (bad, have to add kill)
2. could do something fancy with expiring keys. heartbeat? 
3. every time you move, you update your date. Have a process that goes through and removes old dates every minute (separate from the other stuff). That way it's only on movement. That's pretty easy. I wonder if I can expire the thing too ???
4. Change locations to be something other than hashes ???
    I don't want to mgetall 
    could rooms be sets?

    double lookup to get a room. This call will happen a lot!
    room { userId }
    userId:location

    room { location }
    room:POINT = userId

    room { location, userId }, but that doesn't solve the problem. 

    -- this is what it is
    user:location = point
    room { userId }

    -- then you know everyone to query for. 
    -- you have the same problem, you can't expire the set membership

    it certainly make it easy to expire them. I doubt I can expire set values though!

    -- interesting, I have to maintain all indexes myself :) 

5. Expire the user itself. 

6. Expire something, then get a pubsub from it, and delete. 

7. Require a leave command? 

8. Time on user, scan through and check all old ones?
    - but there's no way to query like that. 


