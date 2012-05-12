// API tests, written in node.js, since I'm still bad at Haskell, I'm on a plane and ghc can't find Network.HTTP for some reason. 

// npm install
// start the server
// npm test

var assert = require('assert')
var request = require('request')
var Port = 3026
var Server = "http://localhost:" + Port

var mongo = require('mongodb-wrapper')

describe('botland api', function() {

    var db = mongo.db('localhost', 27017, 'botland')
    db.collection('bots')

    it('should be on', function(done) {
        request.get(Server + "/version", function(err, request, body) {
            assert.ifError(err)
            assert.ok(body)
            assert.ok(body.length > 0, "Should have gotten back a version from the api")
            done()
        })
    })

    // how? I could connect to mongo myself and do it, that's a little 
    it('should reset the database', function(done) {
        db.bots.remove(done)
    })


    var game = null

    describe('game', function() {

        it('should give game stats', function(done) {
            request.get({url:Server + "/game/info", json:true}, function(err, rs, g) {
                assert.ifError(err)
                assert.ok(g)
                assert.ok(g.width)
                assert.ok(g.height)
                assert.ok(g.tick)
                game = g
                done()
            })
        })

        it('should return an empty world', function(done) {
            request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 0)
                done()
            })
        })
    })

    // save our playerId, so we can use it in later tests
    var playerId = null

    describe("player", function() {
        it("should give me an id", function(done) {
            var player = {name:"test", source:'fake'}
            request.post({url:Server + "/players", json:player}, function(err, rs, data) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, data.message)
                playerId = data
                assert.ok(playerId, 'should have returned an id')
                assert.ok(typeof playerId == "string", 'id was not a string')
                done()
            })
        })
    })

    // keep track of our minionId for later tests
    var minionId = null
    var bot = {x:0, y:0, name:'bot1', sprite:'test'}

    describe('spawn', function() {

        it('should not allow me to spawn off field', function(done) {
            var bot = {x:-1, y:0, name:'offfield', sprite:'test'} 
            request.post({url: Server + "/players/" + playerId + "/minions", json: bot}, function(err, rs, id) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 400, 'missing 400 status code')
                assert.ok(id.message, 'missing error message')
                done()
            }) 
        })

        it('should allow me to spawn a bot', function(done) {
            request.post({url: Server + "/players/" + playerId + "/minions", json: bot}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, body.message)
                minionId = body
                assert.ok(minionId, 'minionId was undefined')
                done()
            })
        })

        it('should error if I try to spawn in the same place twice', function(done) {
            request.post({url: Server + "/players/" + playerId + "/minions", json: bot}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 400)
                assert.ok(body.message)
                done()
            })
        })

        it('should show me in the game', function(done) {
            request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 1, "not showing bot on the map")
                var me = locations[0]
                assert.equal(me.id, minionId)
                assert.equal(me.x, bot.x)
                assert.equal(me.y, bot.y)
                done()
            })
        })
    })

    describe('movement', function() {

        it('should let me move', function(done) {
            var url = Server + "/players/" + playerId + "/minions/" + minionId + "/commands"
            request.post({url: url, json:{action:"Move", direction:"Right"}}, function(err, rs, data) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, data)
                done()
            })
        })

        it('should update the game', function(done) {
            setTimeout(function() {
                request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                    assert.ifError(err)
                    assert.ok(locations)
                    assert.equal(locations.length, 1)
                    var me = locations[0]
                    assert.equal(me.id, minionId)
                    assert.equal(me.x, 1, "did not move right")
                    assert.equal(me.y, 0)
                    done()
                })
            }, game.tick)
        })

        it("should not let me move out-of-bounds", function(done) {
            request.post({url: Server + "/players/" + playerId + "/minions/" + minionId + "/commands", json:{action:"Move", direction:"Up"}}, function(err, rs, data) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, 'should give 200 status code even though the command is invalid')

                setTimeout(function() {
                    request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                        assert.ifError(err)
                        assert.ok(locations)
                        assert.equal(locations.length, 1)
                        var me = locations[0]
                        assert.equal(me.id, minionId)
                        assert.equal(me.x, 1, "did not move right")
                        assert.notEqual(me.y, -1, "moved off the board!")
                        assert.equal(me.y, 0, "not in the same y")
                        done()
                    })
                }, game.tick)
            })
        })
    })

    describe('attack', function() {
        var bot2Id = null
        it('should spawn a second bot', function(done) {
            var bot = {x: 0, y: 0, name: 'bot2', sprite: 'test'}
            request.post({url: Server + "/players/" + playerId + "/minions", json: bot}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, body.message)
                bot2Id = body
                assert.ok(bot2Id, 'minionId was undefined')
                done()
            })
        })

        it('should both appear in the world', function(done) {
            request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 2)
                var b2 = locations[1]
                assert.equal(b2.id, bot2Id)
                done()
            })
        })

        it('should attack bot1', function(done) {
            request.post({url: Server + "/players/" + playerId + "/minions/" + bot2Id + "/commands", json:{action:"Attack", direction:"Right"}}, function(err, rs, data) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200)
                done()
            })
        })

        it('bot1 should be dead', function(done) {
            setTimeout(function() {
                request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                    assert.ifError(err)
                    assert.ok(locations)
                    assert.equal(locations.length, 2)
                    var b1 = locations.filter(function(b) { return b.name == 'bot1' })[0]
                    assert.ok(b1)
                    assert.equal(b1.state, "Dead")
                    done()
                })
            }, game.tick)
        })

        it('should give kill to bot2', function(done) {
            request.get({url:Server + "/minions/" + bot2Id, json:true}, function(err, rs, bot) {
                assert.ifError(err)
                assert.ok(bot)
                assert.equal(bot.kills, 1)
                done()
            })
        })

        it('should remove bot1 after another tick', function(done) {
            setTimeout(function() {
                request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                    assert.ifError(err)
                    assert.ok(locations)
                    assert.equal(locations.length, 1, "did not remove dead unit")
                    done()
                })
            }, game.tick)
        })
    })

    describe('leaderboard', function() {
        // requires the kill stuff right before this
        it('should have kill registered from last attack', function(done) {
            request.get({url:Server + "/top/killers", json:true}, function(err, rs, bots) {
                assert.ifError(err)
                assert.ok(bots)
                assert.equal(bots.length, 1)
                assert.equal(bots[0].kills, 1)
                done()
            })
        })

        it('should track connection times', function(done) {
            request.get({url:Server + "/top/survivors", json:true}, function(err, rs, bots) {
                assert.ifError(err)
                assert.ok(bots)
                assert.ok(bots.length)
                assert.ok(bots[0].created)
                assert.ok(bots[0].id, minionId) // is the oldest
                done()
            })
        })
    })

    describe('cleanup', function() {
        var minionId = null

        it('should start with only bot2', function(done) {
            request.get({url: Server + "/game/objects", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.equal(locations.length, 1, "Should only have 1 bot to start")
                done()
            })
        })

        it('should spawn another bot', function(done) {
            var bot = {x:1, y:1, name:'cleanup', sprite:'test'} 
            request.post({url: Server + "/players/" + playerId + "/minions", json: bot}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, body.message)
                minionId = body
                assert.ok(minionId, 'minionId was undefined')
                done()
            }) 
        })

        it('should delete minionId', function(done) {
            request.del({url: Server + "/players/" + playerId + "/minions/" + minionId, json: true}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, body.message)
                done()
            })
        })

        it('should be gone', function(done) {
            request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 1)
                var b2 = locations[0]
                assert.notEqual(b2.id, minionId)
                done()
            }) 
        })

        it('should delete mcp', function(done) {
            request.del({url: Server + "/players/" + playerId, json: true}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, body.message)
                done()
            })
        })

        it('should be empty', function(done) {
            request.get({url:Server + "/game/objects", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 0)
                done()
            })  
        })
    })
})













