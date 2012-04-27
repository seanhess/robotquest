
$(function() {
    var bots = []

    var MINUTE = 60
    var HOUR = 60*MINUTE
    var DAY = 24*HOUR
    var MONTH = 30*DAY
    var YEAR = 365*DAY

    var $window = $(window)
    var $viewer = $("#viewer")
    var $container = $("#container")

    var PX = 32
    var PY = 32

    var padding = 0

    $window.on('resize', function() {
        console.log("RESIZE")
    })

    // first, get the world
    $.get("/game/info", function(gameInfo) {

        var w = gameInfo.width
        var h = gameInfo.height
        var ms = gameInfo.tick

        // this will make it scroll
        $viewer.css({width: w * PX, height: h * PY})
        $container.css({width: (w+2)*PX, height: (h+2)*PY})

        console.log("GAME INFO", w, h, ms)

        function poll() {
            $.get("/game/objects", function(bots) {
                tick(bots)
            })
        }

        function tick(bots) {

            $(".bot").addClass("inactive")

            bots.forEach(function(bot) {
                var $bot = $("#" + bot.id)

                if (!$bot.length) {
                    console.log("NEW BOT", bot.id, bot.name, bot.x, bot.y, bot.sprite)
                    $bot = botView(bot)
                    $viewer.append($bot)
                }

                $bot.css({left: (bot.x) * PX + padding, top: (bot.y) * PY + padding})
                // $bot.css({width: PX, height: PY})

                if (bot.state === "Dead") {
                    sprite($bot, "effects-6-8")
                }

                $bot.removeClass("inactive")
            })

            $(".bot.inactive").remove()
        }

        function botView(bot) {
            var $bot = $("<a href='/bots/"+bot.id+"' class='bot sprite' id='"+bot.id+"'></a>")
            $bot.data(bot)
            sprite($bot, bot.sprite)
            return $bot
        }

        poll()
        setInterval(poll, ms)
    })

    function sprite($el, s) {

        // compute offset mathematically
        // dragon-3-4

        var parts = s.split('-')

        var sheet = parts[0]
        var xi = parseInt(parts[1], 10)
        var yi = parseInt(parts[2], 10)

        $el.addClass(sheet)
        $el.css({'background-position': '-' + xi*32 + 'px -' + yi*32 + 'px'})       
    }

    function age(created) {
        var date = new Date(created)
        var ds = Math.floor((Date.now() - date.getTime())/1000)

        if (ds < MINUTE) return ds + "s"
        if (ds < HOUR) return Math.floor(ds/MINUTE)  + "m"
        if (ds < DAY) return Math.floor(ds/HOUR) + "h"
        return Math.floor(ds/DAY) + "d"
    }


    // ROUTING //////////////////////////////////////////////
    $(window).bind('hashchange', function() {

        var hash = window.location.hash

        if (!hash) return hideBot()

        var matchBot = hash.match(/#\/bots\/(\w+)/)
        if (matchBot) {
            var botId = matchBot[1]
            var $bot = $("#" + botId)
            var bot = $bot.data()
            showBot(bot)
        }
    })

    // BOT INFORMATION /////////////////////////////////////
    var $botInfo = $("#botInfo")

    function showBot(bot) {
        $botInfo.show()
        $botInfo.find(".name").text(bot.name)
        $botInfo.find(".age").text(age(bot.created))
        $botInfo.find(".kills").text(bot.kills)
        $botInfo.find(".player").text(bot.player)

        sprite($botInfo.find(".sprite"), bot.sprite)

        // get player information
        $.get('/players/' + bot.player, function(player) {
            $botInfo.find(".source").attr('href', player.source)
        })

        // TODO get player information, add it hereszzz
        // TODO change bot: player (drop source)
    }

    function hideBot() {
        $botInfo.hide()
    }

    $viewer.on("click", ".bot", function(event) {
        var bot = $(this).data()
        //showBot($(this).data())
        window.location.hash = "/bots/" + bot.id
        return false
    })

    // only outside clicks
    $botInfo.on("click", function(e) {
        window.location.hash = ""
    })



    // LEADERBOARDS 
    var $killers = $("#killers")
    var $survivors = $("#survivors")
    var $rowTemplate = $survivors.find(".row.template").remove().clone()
    var $headerTemplate = $survivors.find(".header.template").remove().clone()


    function loadLeaderboards() {
        $.get('/top/killers', function(bots) {
            $killers.html($headerTemplate.clone())
            bots.forEach(function(b) {
                var $view = leaderboardRow(b)
                $killers.append($view)
            })
        })

        $.get('/top/survivors', function(bots) {
            $survivors.html($headerTemplate.clone())
            bots.forEach(function(b) {
                var $view = leaderboardRow(b)
                $survivors.append($view)
            })
        })
    }

    function leaderboardRow(b) {
        var $view = $rowTemplate.clone()
        $view.find(".kills").text(b.kills)
        $view.find(".age").text(age(b.created))
        $view.find(".name").text(b.name).attr('href', "#/bots/" + b.id)
        return $view
    }

    loadLeaderboards()
    var slowInterval = setInterval(loadLeaderboards, 10*1000)
})
