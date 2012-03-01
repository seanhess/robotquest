Botland
=============================


TODO

[x] Get it compiling
[-] test to see if you can use raise/rescue instead of case statements /fault (auto fault with error)
[x] functional tests
[x] send back a starting location
[x] safe spawn
[x] block edge of the world movement 
[ ] test movement (move, spawn loc, edge)
[ ] unit cleanup (heartbeat?)

[ ] description - home page / repository
[ ] description - robot description fields

[ ] build a world viewer!
    [ ] cors support?

[ ] build a bot!
    [ ] cors?

SAFE SPAWN
    [ ] or support multiple things being in a location.. ... hmm... 

OTHER WAYS TO REPRESENT:
    a room is a set of users. they can leave a room, etc

    locations: id + location
    set: {id location}

    room:{x:5,y:10} = unitId -- no hash. For checking locality: what's there?
    room = {location} -- these are used locations. You can see WHO is where by checking room:point

    but then you can't get the contents of the room

    -- so it would be a little annoying to get the contents of the room:
    1. get locations
    2. mget keys - gives you the unit ids
    3. map them together?

    -- what's the best way to track movement?
    -- in theory, you could enter a square with something on it, no? (NOT NECESSARILY) 
    -- I can decide. depends on which is easier

    -- 1 -- feed the room with available locations and pop them off (have to feed it)
    -- 2 -- random (hate it, because you have to keep trying. Indeterminate)
    -- 3 -- 0, 0 is a special square. You don't exist till you move off it (easy!) -- only the last person to move there can. ok. so leave it like it is. You don't exist unless the system says you do.
        -- prevent ANYONE from existing until they step off the start square 0, 0

EXPLORE
[ ] middleware validate user auth token (or cleaner way to do it)

>>> Do me first <<<
[ ] Functional Tests
    [ ] They MUST be at the API level. 
    [ ] Use the production system, just assume you don't run them in production.

[ ] Pick a string representation
	[ ] choose either Text or ByteString and stick with it. 
	[ ] some of these libraries can vary. like scotty can pull anything out of a param

[ ] Get actorMove cleaner

CLEANUP

[ ] 
