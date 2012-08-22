[viewer]: http://robotquest.tk/viewer/ "RobotQuest Viewer"
[source]: http://github.com/seanhess/robotquest "RobotQuest Source"

![RobotQuest Screenshot](https://raw.github.com/seanhess/robotquest/master/public/screen.png)

What is RobotQuest?
===================

RobotQuest is a MMO, programming game. Instead of playing RobotQuest directly, you write a program that plays it for you. Your program communicates with the [game server API](#api) over HTTP by sending and receiving JSON messages.

All players share a single world

[The Viewer][viewer] lets you watch the game being played live.

[The Documentation](#documentation) will teach you how to connect to the game and control your minions.

[The Source](http://github.com/seanhess/robotquest) is here on github. Fork it, read it, or yell at me on the [issues page](http://github.com/seanhess/robotquest/issues). The server is written in Haskell, and the AI is written in CoffeeScript

What can I do?
--------------

You can create minions (heroes or monsters), move them, and attack things near you. There is an AI player that spawns minions of increasing degrees of danger to give you trouble.

What's the goal?
----------------

You can see who has the most kills and has survived the longest on the [Viewer][viewer]. Other than that it's up to the players. The game will change signifiantly depending on the programs written for it and as new features are added.

Rules
-----

Please respect the following rules. The server will eventually enforce them.

1. **Spawn with purpose** - The API allows you to spawn multiple minions per player. Explore getting minions to work together, but don't spawn one on every square or anything. Consider having internal rules, like you can only spawn a minion when you kill one.

2. **Single player per program**- You can try many different approaches to the game, but idea is that you write a program, which is a player. If you want to try a different approach, feel free to turn your old program off and use the same player name, or use a different player name, but don't spam player accounts

Feature Ideas
-------------

* Blocks - Place walls down on the map that block movement
* Gold -  Collect gold
* Store - Spend gold to buy items (upgrades)
* Trading - Swap gold or items with other minions
* Big World - Infinite or much larger world. Safe zones?
* Limit Player Creation - right now it's completely open
* Limit Spawning - make it a resource of some kind

Documentation
=============

### <a href="#examples">Examples</a>

### <a href="#types">Types</a>

<ul>
  <li><a href="#fault">Fault</a>
  <li><a href="#id">Id</a>
  <li><a href="#ok">Ok</a>
  <li><a href="#gameinfo">GameInfo</a>
  <li><a href="#player">Player</a>
  <li><a href="#minion">Minion</a>
  <li><a href="#command">Command</a>
</ul>

### <a href="#routes">Routes</a>

<ul>
  <li><a href="#get-gameinfo">GET /game/info</a>
  <li><a href="#get-gameobjects">GET /game/objects</a>
  <li><a href="#post-players">POST /players</a>
  <li><a href="#get-playersname">GET /players/:name</a>
  <li><a href="#post-playersplayeridminions">POST /players/:playerId/minions</a>
  <li><a href="#get-minionsminionid">GET /minions/:minionId</a>
  <li><a href="#post-playersplayeridminionsminionidcommands">POST /players/:playerId/minions/:minionId/commands</a>
  <li><a href="#delete-playersplayerid">DELETE /players/:playerId</a>
  <li><a href="#delete-playersplayeridminionsminionid">DELETE /players/:playerId/minions/:minionId</a>
</ul>

### <a href="#sprites">Sprites</a>

Notes
-----

All types sent and received from the server are in JSON. Don't forget to set "Content-Type: application/json" and Content-Length (good libraries will do this for you)

This includes the [Id](#id) type, which is just a JSON string. Run everything through your JSON parser and all will be well

Examples
--------

These examples are in pseudocode. For a complete example <a href="https://github.com/seanhess/robotquest/blob/master/ai/app.coffee">please take a look at the AI (CoffeeScript)</a>

### Control a minion

    # register our player
    player = {name:"example", source: "http://github.com/seanhess/robotquest"}
    player.id = POST "/players" player

    # spawn a minion
    minion = {x: 0, y: 5, name:"example1", sprite: "dragon-1-3"}
    minion.id = POST "/players/$player.id/minions" minion

    # move our minion
    POST "/minions/$player.id/minions/$minion.id" {action: "Move", direction: "Right"} 

    # remove our minion
    DELETE "/minions/$player.id/minions/$minion.id"

### See what is going on

    game = GET "/game/info"
    repeatEvery game.tick 
      objects = GET "/game/objects"
      for minion in objects
        print minion


Types
-----

### Fault

    { message: "Space occupied" }

Any method can return a <code>Fault</code> instead of its regular response.

### Ok

    "Ok"

### Id

    "6dc21b03a79fa15d"
    
Note that this will include quotes when it comes down. This is valid JSON. Just run it through your normal JSON decoder and it will come out a string

### GameInfo

    {
        width: 25,
        height: 20,
        tick: 1000
    }

`tick` - Game tick in ms. How often you can send commands and poll for information

<code>width, height</code> - World dimensions in squares

### Player

    {
        name: "sean",
        source: "http://github.com/seanhess/robotquest"
    }

<code>name</code> - A unique player name for your program.
<code>source</code> - Bot homepage. Source code preferred.

### Minion

    // To Server
    {
      name: "rat",
      sprite: "monster1-1-4",
      x: 10,
      y: 10
    }
      
    // From Server
    {
      name: "rat",
      sprite: "monster1-1-4",
      x: 10
      y: 10,

      // generated fields
      state: "Active",
      kills: 0,
      created: "2012-05-03T12:06:48.666Z",
      player: "AI",
      id: "6dc21b03a79fa15d",
    }

<p><code>name</code> - The name chosen for the minion by its player.</p>
<p><code>sprite</code> - Player-chosen <a href="#sprites">sprite</a></p>
<p><code>x, y</code> - Position in squares</p>
<p><code>state</code> - Active or Dead. If a minion dies, the server will send down Dead once, then the minion will no longer appear in the result of <a href="#game_objects">/game/objects</a></p>
<p><code>player</code> - Name of the controlling <a href="#player">Player</a></p>

### Command
<p>Careful, I'm case sensitive!</p>

    {
        action: "Move",
        direction: "Left"
    }
<p><code>action</code> - "Move", "Attack"</p>
<p><code>direction</code> - "Left", "Right", "Up", or "Down"</p>


Routes
------

Each route lists the url, the body it expects (if any), and what it returns. All types are JSON

<h3 id="game_info">GET /game/info</h3>
Always call this when you start. Respect the tick and size
<pre><span>returns</span> <a href="#gameinfo">GameInfo</a></pre>

<h3 id="game_objects">GET /game/objects</h3>
<p>Call this every <code>tick</code> to know where things are in the game. If a minion dies, it will come back one last time with <code>state: "Dead"</code>, after which it will stop appearing in this call.</p>
<div>
  <pre><span>returns</span> [<a href="#minion">Minion</a>]</pre>
</div>


<h3 id="new_player">POST /players</h3>

<p>Register your player. Note that the id returned from this is <em>secret</em>. You use it to <a href="#new_minion">spawn</a> and <a href="#command">command</a> your minions.</p>

<div>
  <pre><span>body</span> <a href="#player">Player</a></pre>
</div>

<div>
  <pre><span>returns</span> <a href="#id">Id</a></pre>
</div>
  
<h3 id="get_player">GET /players/:name</h3>

<p>Info about another player</p>

<div>
  <pre><span>returns</span> <a href="#player">Player</a></pre>
</div>



<h3 id="new_minion">POST /players/:playerId/minions</h3>

<p>Spawn a minion on the map</p>

<div>
  <pre><span>body</span> <a href="#minion">Minion</a> - only send {name, sprite, x, y}</pre>
</div>

<div>
  <pre><span>returns</span> <a href="#id">Id</a></pre>
</div>



<h3 id="get_minion">GET /minions/:minionId</h3>

<p>All available details for a minion.</p>

<div>
<pre><span>returns</span> <a href="#Minion">Minion</a></pre>
</div>


<h3 id="command">POST /players/:playerId/minions/:minionId/commands</h3>

<p>Issue a command to one of your minions. The command will be executed at the next game tick. If you issue more than one command per tick it will replace your previous command.</p>

<div>
  <pre><span>body</span> <a href="#command">Command</a></pre>
</div>

<div>
  <pre><span>returns</span> <a href="#ok">Ok</a></pre>
</div>


<h3 id="delete_player">DELETE /players/:playerId</h3>

<p>Remove your player</p>

<div>
  <pre><span>returns</span> <a href="#ok">Ok</a></pre>
</div>

<h3 id="delete_minion">DELETE /players/:playerId/minions/:minionId</h3>

<p>Remove one of your minions</p>

<div>
  <pre><span>returns</span> <a href="#ok">Ok</a></pre>
</div>




<h2 id="sprites">Sprites</h2>

I am using the extraordinarily complete, free, <a href="http://pousse.rapiere.free.fr/tome/tome-tiles.htm">angbandtk dungeon tileset</a>. When creating a minion, you can choose the sprite that represents it. The format is:

  <pre>sheet-x-y</pre>

where <code>sheet</code> is the name of one of the following sheets, and <code>x</code> and <code>y</code> are the 0-indexed offset from the top left. For example, the dark blue ghost is <code>undead-0-0</code>, while the phoenix is <code>uniques-8-5</code>

<h5>classm</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_classm32.gif">

<h5>humans</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_humans32.gif">

<h5>undead</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_undead32.gif">

<h5>uniques</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_uniques32.gif">

<h5>monster5</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster532.gif">

<h5>monster1</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster132.gif">

<h5>monster2</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster232.gif">

<h5>monster3</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster332.gif">

<h5>monster4</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster432.gif">

<h5>monster6</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster632.gif">

<h5>monster7</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_monster732.gif">

<h5>people</h5>
<img src="https://raw.github.com/seanhess/robotquest/master/public/assets/angbandtk/dg_people32.gif">


