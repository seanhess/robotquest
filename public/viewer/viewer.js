
$(function() {
    var bots = []

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
    $.get("/game", function(gameInfo) {

        var w = gameInfo.width
        var h = gameInfo.height
        var ms = gameInfo.tick

        // this will make it scroll
        $viewer.css({width: w * PX, height: h * PY})
        $container.css({width: (w+2)*PX, height: (h+2)*PY})

        console.log("GAME INFO", w, h, ms)

        function poll() {
            $.get("/game/minions", function(bots) {
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

    // require moment.js
    function age(created) {
        return moment(created).fromNow().replace(/ (minute|second|day|month|year)(s?) ago/, function(match, name, s){
            if (name == "minute") return 'm'
            else if (name == "second") return 's'
            else if (name == "hour") return 'h'
            else if (name == "month") return 'month'+s
            else if (name == "year") return 'year'+s
            return "(((" + name +"|" + match + ")))"
        })
    }

    // BOT INFORMATION
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

    $viewer.on("click", ".bot", function(event) {
        showBot($(this).data())
        return false
    })

    // only outside clicks
    $botInfo.on("click", function(e) {
        $botInfo.hide()
    })




    // LEADERBOARDS 
    var $killers = $("#killers")
    var $survivors = $("#survivors")
    var $rowTemplate = $survivors.find(".row.template").remove().clone()
    var $headerTemplate = $survivors.find(".header.template").remove().clone()


    console.log("HI")
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
        $view.find(".name").text(b.name)
        return $view
    }

    loadLeaderboards()
    var slowInterval = setInterval(loadLeaderboards, 10*1000)
})
