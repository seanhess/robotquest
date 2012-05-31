### 
Single bot for debugging
###

request = require 'request'

HOST = process.env.HOST || "http://localhost:3026"
NAME = "debug"
REPO = "http://github.com/seanhess/botland"

start = (host) ->

  player =
    name: NAME
    source: REPO

  # standard error handling 
  # should cause everything to exit
  # OS will respawn it
  onError = (err) -> console.log "ERROR", err.message

  api = robotQuestApi host, onError

  request

  ## START 
  api.gameInfo (info) ->

    api.createPlayer player, (id) ->
      player.id = id

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

        # if there are fewer than MONSTERS objects, then make some AI!
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

## REUSABLE AI

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

eq = curry (a, b) -> a == b
id = (obj) -> obj.id


if module == require.main
  start HOST
  console.log "STARTED AI. HOST=#{HOST}"
  
## When you command something that doesn't exist any more you get a not authorized


