### 
RobotQUEST AI
###

# On any error: I want to log the error, then exit and reconnect. (Throw the error)

request = require 'request'
{clone, map, find, compose, isEqual, bind, extend, filter, intersect, sortBy} = require 'underscore'
{curry} = require 'fjs'

HOST = process.env.HOST || "http://localhost:3026"
AINAME = "AI"
REPO = "http://github.com/seanhess/botland"

MONSTERS = process.env.MONSTERS || 50

start = (host) ->

  player =
    name: AINAME
    source: REPO

  bots = []

  # standard error handling 
  # should cause everything to exit
  # OS will respawn it
  onError = (err) -> console.log "ERROR", err.message

  api = robotQuestApi host, onError

  ## START 
  api.gameInfo (info) ->

    api.createPlayer player, (data) ->
      player.id = data.id

      poll = ->
        api.objects (objects) ->
          tick objects

      setInterval poll, info.tick

      ## MONSTER ACTONS
      # api: the api
      # objects: the world
      # player: the player
      # bot: the bot
      # info: the game info
      act = (objects, bot) ->
        ai = find ais, (a) -> a.name() is bot.name
        ai.act api, info, player, objects, bot

      ## MAIN GAME
      # objects: the world
      tick = (objects) ->

        # update all our bots with info from the server
        bots = objects.filter(isAi).map (newBot) ->
          bot = find bots, (b) -> b.id is newBot.id
          extend bot ? {}, newBot

        if bots.length < MONSTERS
          x = random info.width
          y = random info.height
          type = randomElement ais
          spawn(x, y, type.sprite(), type.name())

        bots.forEach (bot) ->
          act objects, bot

      # SPAWN
      spawn = (x, y, sprite, name) ->
        bot = {x, y, sprite, name}
        api.createMinion player, bot, ->


## HELPERS
isAi = (bot) -> bot.player == AINAME

random = (n) -> Math.floor(Math.random() * n)
randomElement = (vs) -> vs[random(vs.length)]

## AI!

# RAT (boring little guys, they never attack, they don't move that much!)
rat =
  name: -> "rat"
  sprite: -> randomElement ["monster1-0-4", "monster1-1-4", "monster1-2-4", "monster1-3-4"]
  act: (api, info, player, objects, bot) ->
    api.command player, bot, wander(), ->


# ORC: will sometimes attack you if you are next to it for 2 turns 
# they are slow, they take an extra turn to hit you, only if you are still next to them
orc =
  name: -> "orc"
  sprite: -> randomElement ["monster1-0-2", "monster1-1-2", "monster1-5-1"]
  act: (api, info, player, objects, bot) ->
    targets = filter objects, adjacent(bot)

    targetIds = map targets, id
    slowTargetIds = intersect bot.oldTargetIds, targetIds

    command = if slowTargetIds.length
      # attack them!!!
      slowTarget = find targets, (b) -> b.id is slowTargetIds[0]
      attack(navigate(bot, slowTarget))
    else
      wander()

    bot.oldTargetIds = targetIds

    api.command player, bot, command, ->


# BLARG: Wanders, but attacks perfectly if something comes near
blarg =
  name: -> "blarg"
  sprite: -> randomElement [
    "monster2-2-6", "monster2-3-6", "monster2-4-6", "monster2-5-6"
    "monster2-0-7", "monster2-1-7", "monster2-2-7", "monster2-3-7", "monster2-4-7", "monster2-5-7",
    "monster2-0-8", "monster2-1-8", "monster2-2-8", "monster2-3-8", "monster2-4-8", "monster2-5-8"
  ]
  act: (api, info, player, objects, bot) ->
    targets = filter objects, adjacent(bot)

    command = if targets.length > 0
      attack(navigate(bot, targets[0]))
    else wander()

    api.command player, bot, command, ->


# GOOBER: Picks the nearest target within 3 spaces or so, then attacks
# TODO fix oscillation by allowing a NONE direction, in addition to a stop action
demon =
  name: -> "demon"
  sprite: -> randomElement ["monster1-0-5", "monster1-1-5", "monster1-2-5", "monster1-3-5", "monster1-4-5", "monster1-5-5"]
  act: (api, info, player, objects, bot) ->

    ds = map objects, (b) ->
      bot: b
      distance: distance(bot, b)

    # this will also prevent you from picking yourself
    ds = ds.filter (obj) -> 0 < obj.distance < 3

    ds = sortBy ds, (obj) -> obj.distance

    target = ds[0]

    command = if target?
      dir = navigate bot, target.bot
      if target.distance is 1 then attack dir
      else move dir
    else wander()

    api.command player, bot, command, ->

sorcerer =
  name: -> "sorcerer"
  sprite: -> "monster1-4-1"
  act: (api, info, player, objects, bot) ->

    leaders = sortBy objects, (b) -> b.kills

    target = leaders[0]

    command = if target?
      dir = navigate bot, target
      if adjacent bot, target then attack dir
      else move dir
    else wander()

    api.command player, bot, command, ->

# SLUDGE: umm... 

# MAGE: will hunt down the person with the most kills. At the top of the leaderboard :) Booyah!
  # once it acquires a target it will NEVER give up!
  # you must destroy it!

# DRAGON: never moves. Attacks anything near it immediately. 

ais = [rat, rat, rat, rat, orc, orc, blarg, blarg, demon, sorcerer]
#ais = [orc, demon, sorcerer]
#ais = [sorcerer]

## REUSABLE AI

UP = "Up"
DOWN = "Down"
LEFT = "Left"
RIGHT = "Right"

STOP = "Stop"
ATTACK = "Attack"
MOVE = "Move"

directions = [UP, DOWN, LEFT, RIGHT]

# if two objects are adjacent
# functional programming example! This works against ANY object that has x and y coordinates!
# I don't have to be over-specific
adjacent = curry (a, b) ->
  dirs = directions.map (d) -> dir(b, d)
  hits = dirs.filter isHit(a)
  hits.length

# move point in direction
dir = (point, d) ->
  switch d
    when UP then {x: point.x, y: point.y-1}
    when DOWN then {x: point.x, y: point.y+1}
    when LEFT then {x: point.x-1, y: point.y}
    when RIGHT then {x: point.x+1, y: point.y}
    else point

# gives you a direction from a to b
# assumes they are adjacent
navigate = (a, b) ->
  if a.x is b.x
    if a.y < b.y then DOWN
    else UP
  else
    if a.x < b.x then RIGHT
    else LEFT


pointKey = (p) -> p.x + "," + p.y

distance = curry (a, b) -> Math.abs(b.x - a.x) + Math.abs(b.y - a.y)

mask = curry (fields, obj) ->
  masked = {}
  fields.forEach (f) ->
    masked[f] = obj[f]
  return masked

isHit = curry (a, b) -> a.x is b.x and a.y is b.y

wander = ->
  direction = randomElement directions
  action = randomElement ["Stop", "Stop", "Move"]
  {action, direction}


attack = (direction) -> {action: ATTACK, direction}

move = (direction) -> {action: MOVE, direction}

stop = (d) -> {action: STOP, direction: UP}

val = curry (key, obj) -> obj[key]
eq = curry (a, b) -> a == b
id = (obj) -> obj.id


## API
robotQuestApi = (host, onError) ->

  respond = (cb, checkStatus = true) ->
    (err, rs, body) ->
      if err? then return onError err
      if checkStatus and rs.statusCode != 200
        return onError new Error body.message
      cb body

  gameInfo: (cb) ->
    request.get {url: host + "/game/info", json: true}, respond cb

  objects: (cb) ->
    request.get {url: host + "/game/objects", json: true}, respond cb

  createPlayer: (player, cb) ->
    request.post {url: host + "/players", json: player}, respond cb

  createMinion: (player, minion, cb) ->
    request.post {url: host + "/players/" + player.id + "/minions", json: minion}, respond(cb, false)

  command: (player, minion, command, cb) ->
    #console.log "COMMAND", minion.id, command
    request.post {url: host + "/players/" + player.id + "/minions/" + minion.id + "/command", json: command}, respond cb



if module == require.main
  start HOST
  
## When you command something that doesn't exist any more you get a not authorized

