### 
RobotQUEST AI
###

# On any error: I want to log the error, then exit and reconnect. (Throw the error)

{map, find, extend, filter, intersect, sortBy, last, id} = require 'underscore'

lib = require './lib'
{robotQuestApi, adjacent, dir, navigate, random, randomElement, id} = lib
{UP, DOWN, LEFT, RIGHT, STOP, ATTACK, MOVE} = lib
{directions, pointKey, distance, isHit, attack, move, stop} = lib


HOST = process.env.HOST || "http://localhost:3026"
AINAME = "AI"
REPO = "http://github.com/seanhess/botland"

MONSTERS = process.env.MONSTERS || 10

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
    target = last leaders

    command = if target? and target.id != bot.id
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

wander = ->
  direction = randomElement directions
  action = randomElement ["Stop", "Stop", "Move"]
  {action, direction}

if module == require.main
  start HOST
  console.log "STARTED AI. HOST=#{HOST}"
  
## When you command something that doesn't exist any more you get a not authorized

