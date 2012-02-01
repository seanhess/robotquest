# Botland

An open environment and rest API for uploading bots which can interact with each other.

## Architecture

1. Should maintain state in memory, in haskell (this will force me to learn haskell better, but will also make the project unscalable.). Could persist to disk? How would haskell even begin to share state between processes?

1b. Maintain state in redis. Perhaps redis has some great ways to query. This is more scalable. Allows me to have multiple app servers, but I'm not sure how to deal with consistency / concurrency. If two guys move on to the same space, who gets it? How to deal with locks? I think this actually makes more sense long-term

1c. Use whatever db has the friendliest modules for haskell. MongoDB, Redis, (Riak?) I'm not sure Riak is what I want. Redis has some poll-oriented libraries, it seems. We'll see

### API

1. /unit/:create - connects a new creature, gives you the token to access. Expects a heartbeat every so often or it will disconnect you? No, they always stay there until destroyed or they move. It seems short-sighted to assign all information about the unit to the connection though. They'll need to send their connection id via header

    - returns a connection id
    - returns a unit id

2. /unit/:unitId/moveTo POST {position:Position}

Does my REST api have commands for "move" or to "set position". "set position" is more rest-y, but it doesn't handle security very well. I COULD allow them to set their position, and then check to see if it is a valid move or not. Ok, so I can design it either way. It's easier to think in terms of actions, rather than entities you are manipulating. 

### Actions

Move: could be set position
Say: could be "set current thing I'm saying", but that's just weird. 
Attack: could be "set mode to 'attack unit X'"
Defend: global, say "set defense on" etc. 

### These action modes should be ongoing? Or it should report "finished" once you move there or something?

PUT /user/:unitId/action/:action sets the user's current action to X

Move to 100,100. Could take 10 ticks, during which you could be interrupted. Either way I'm going to have to send state down to the client, but that can be a separate path from the response. It should just be acknowledge action change, then it will tell you through your state response.


### State Pushes

1. A separate connection from commands
2. Polling (keep it over http for simplicity)
3. Only your immediate vicinity. (needs location nearness on data store)
4. Only send state changes? Or re-send your immediate state every tick?
5. Per "room". It sends you the state for the room you are in. you can move to another room, and start getting the state changes there. 
    - start with full state pulls (completely uncacheable)
    - return the state of the entire room

6. Ticks. "Give me updates from tick 5 to 6". and from 6 to 7. If any don't come back you know something is messed up, and you can rearrange the order later. It makes updates easy too. At any point you can re-sync. Or you can ask for updates from 5 to 7. No problemz. It's still cacheable so long as any number of clients are asking for the same information

7. Tick period and poll interval. It doesn't make sense to have a million ticks and a slow poll interval, right? I'd guess a factor of 2x ticks per poll interval will suffice? Can they be chosen independently? I'd love to have the server evaluating at 10x a second. 
    - can I make the server completely event based instead? or is that confusing?
    - I could just let the natural order in the database take over. 
    - this is one of the most important decisions.

### Rooms

1. Rooms are a geographical area. You move into the next one then start asking for changes in THAT area. You may need to get bordering areas so you don't have solid boundaries. 

2. Can also be indoor rooms. Like zelda. Geographical space that exists outside the area of the main grid. Can be bigger on the inside than out. You'd receive updates for that room all at once. Hard boundary on those though (you wouldn't get updates unless you were inside it)


### Protocol

1. Doesn't matter that it be particularly low-bandwidth. We can use gzip later, and the http headers will mean we're large-bandwidth anyway. The only issue is the world-state refreshes, but they should be pretty simple at first anyway.

2. State: {world:[{type:"creature", x: 10, y: 15 }]}


### Coordinate System

1. Starts at 0,0 center of the world, I guess, then goes negative and positive.

### Client-side code

Can I write my javascript in haskell? Probably not, but that would be awesome. Especially if I can include serverside modules, etc. It would get weird once you need to interact with the dom though. Still, would be REALLY nice to get my typing. But stil, probably not worth it. 

### Milestones

Milestone one
1. API — create a unit. 
    [ ] Serialize (try fromJSON CreatureType)
    [ ] Create in redis (any way you want for now!)
2. API — move a unit
3. API — get world state
4. Web Client — Show entire world state, and refresh regularly via ajax/json.
5. Simple bot that moves around randomly. Can spawn multiple instances of them. Maybe I have a control panel for it?






NEXT STEPS
[√] save a creature into redis any way you want
[√] give the creature a unique id and token and send it back
[√] give it a location
[√] Allow movement
[√] Unique location
[ ] make a client to show the location





        -- 1 -- Hash: x.y -> unitId 
        -- 2 -- roomX -> unitId -- not very useful. would have to have all the position information too
            -- yeah, just do the hash

        -- I could have a set of available locations, and a set of used locations
        -- interesting. o 
            -- it would make it easy to give them a location :)
            -- doesn't 
            -- per-room. Doesn't tell you WHO is where?
        -- set of available locations
        -- set of unavailable locations? no, a set of people's locations
        -- single set of everything
            -- Sets = 1. check set of available locations, 2. claim new location. 3. remove old location 
            -- Hashes = 1. check is available. 2. set new location. 3. remove old location. 
            -- it's the same!

            -- you can still get everything. ok. start with sets. 
            -- this means I'd have to start by filling it up, which is LAME
                -- but I could just fetch it, if it doesn't exist, fill it? 
                -- nawww... it's a little weird
            -- always start them on 0 and don't check for now. 

            -- Hashets

