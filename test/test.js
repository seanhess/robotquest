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


    describe('game', function() {
        it('should return an empty world', function(done) {
            request.get({url:Server + "/game/locations", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 0)
                done()
            })
        })
    })

    // save our mcpId, so we can use it in later tests
    var mcpId = null

    describe("mcp", function() {
        it("should give me a token", function(done) {
            request.post({url:Server + "/mcps", json:true}, function(err, rs, data) {
                assert.ifError(err)
                mcpId = data.id
                assert.ok(mcpId, 'should have returned an id')
                done()
            })
        })
    })

    // keep track of our botId for later tests
    var botId = null
    var bot = {x:0, y:0, name:'test', source:'test', color:"#F00"}

    describe('spawn', function() {

        it('should allow me to spawn a bot', function(done) {
            request.post({url: Server + "/mcps/" + mcpId + "/bots", json: bot}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200, body.message)
                botId = body.id
                assert.ok(botId, 'botId was undefined')
                done()
            })
        })

        it('should error if I try to spawn in the same place twice', function(done) {
            request.post({url: Server + "/mcps/" + mcpId + "/bots", json: bot}, function(err, rs, body) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 400)
                assert.ok(body.message)
                done()
            })
        })

        it('should show me in the game', function(done) {
            request.get({url:Server + "/game/locations", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 1)
                var me = locations[0]
                assert.equal(me.id, botId)
                assert.equal(me.x, bot.x)
                assert.equal(me.y, bot.y)
                done()
            })
        })
    })

    describe('movement', function() {
        it("should out-of-bounds error", function(done) {
            request.put({url: Server + "/mcps/" + mcpId + "/bots/" + botId + "/action", json:{action:"Move", direction:"Left"}}, function(err, rs, data) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 400, 'missing 400 status code')
                assert.ok(data.message, 'missing error message')
                done()
            })
        })

        it('should let me move', function(done) {
            request.put({url: Server + "/mcps/" + mcpId + "/bots/" + botId + "/action", json:{action:"Move", direction:"Right"}}, function(err, rs, data) {
                assert.ifError(err)
                assert.equal(rs.statusCode, 200)
                done()
            })
        })

        it('should update the game', function(done) {
            request.get({url:Server + "/game/locations", json:true}, function(err, rs, locations) {
                assert.ifError(err)
                assert.ok(locations)
                assert.equal(locations.length, 1)
                var me = locations[0]
                assert.equal(me.id, botId)
                assert.equal(me.x, 1)
                assert.equal(me.y, 0)
                done()
            })
        })
    })
})













