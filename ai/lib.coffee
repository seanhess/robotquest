
request = require 'request'
{map, filter} = require 'underscore'
{curry} = require 'fjs'

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
    request.post {url: host + "/players/" + player.id + "/minions/" + minion.id + "/commands", json: command}, respond cb

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

## HELPERS

random = (n) -> Math.floor(Math.random() * n)
randomElement = (vs) -> vs[random(vs.length)]

UP = "Up"
DOWN = "Down"
LEFT = "Left"
RIGHT = "Right"

STOP = "Stop"
ATTACK = "Attack"
MOVE = "Move"

directions = [UP, DOWN, LEFT, RIGHT]


pointKey = (p) -> p.x + "," + p.y

distance = curry (a, b) -> Math.abs(b.x - a.x) + Math.abs(b.y - a.y)

isHit = curry (a, b) -> a.x is b.x and a.y is b.y

attack = (direction) -> {action: ATTACK, direction}

move = (direction) -> {action: MOVE, direction}

stop = (d) -> {action: STOP, direction: UP}

id = (a) -> a.id


module.exports = {robotQuestApi, adjacent, dir, navigate, random, randomElement, UP, DOWN, LEFT, RIGHT, STOP, ATTACK, MOVE, directions, pointKey, distance, isHit, attack, move, stop, id}

