/*
 Copyright (C) 2019 Jan-Dirk van Dingenen
 
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

jQuery.extend( jQuery.easing, {
    easeInQuint: function (x, t, b, c, d) {
        return c*(t/=d)*t*t*t*t + b;
    }
});

$(document).ready(function(){        
    var cpuSpeed = 1000;
    var playerCount = 4;
    var playerName = 'You';
    var endRoundSpeed = cpuSpeed;
    var volume = 1;
    
    function rojo() {
        const BLACK = 'black';
        const WHITE = 'white';
        const BLUE = 'blue';
        const YELLOW = 'yellow';
        const RED = 'rojo';
        const DISCARD = 'discard';
        const CENTER = 'center';
        const FIRST = 'first';

        var colours = [BLACK,WHITE,BLUE,YELLOW,RED];
        var factories = [];
        var bag = [];
        var allTiles = [];
        var discard = [];
        var players = [];    

        var pattern = [ [BLUE,YELLOW,RED,BLACK,WHITE],
                        [WHITE,BLUE,YELLOW,RED,BLACK],
                        [BLACK,WHITE,BLUE,YELLOW,RED],
                        [RED,BLACK,WHITE,BLUE,YELLOW],
                        [YELLOW,RED,BLACK,WHITE,BLUE]];
        var overflowPoints = [-1,-1,-2,-2,-2,-3,-3];
        var zIndexCounter = 0;

        var firstPlayer = 0;
        var activePlayer = firstPlayer;
        var fromFactory = null;
        var dragging = [];
        var dragColour = null;    
        var firstTokenTaken = false;
        var gameEnding = false;
        var droppingTile = false;
        var roundNumber = 0;
        
        var sound;

        class Factory {
            constructor() {
                this.id = factories.length;
                this.tiles = [];
                this.isCenter = false;
                if(this.id == 0) {
                    this.isCenter = true;
                }
                else {
                    this.refill();
                }
                factories.push(this);
            }

            refill() {
                for(var t=0; t < 4; t++) {
                    if(bag.length == 0) {
                        bag = bag.concat(discard);
                        discard = [];
                    }

                    if(bag.length > 0) {
                        var addTile = Math.floor(Math.random() * bag.length);
                        this.tiles.push(bag[addTile]);
                        bag.splice(addTile,1);
                    }                
                }            
            }

            refillAndUpdate() {
                this.refill();
                this.update();
            }

            update() {
                if(this.id == 0) {
                    $('#center').html(this.updateHTML());
                }
                else {
                    $('#factory'+this.id).html(this.updateHTML());
                }                           
                makeTilesDraggable();
            }

            updateHTML() {
                var returnHTML = '';
                for(var t of this.tiles) {      
                    if(t.colour) {
                        returnHTML += '<div class="tileFilled_true tile '+t.colour+'" tileColour="'+t.colour+'" tileId="'+t.id+'"></div>';
                    }
                    else {
//                        console.log(t,this.tiles,bag,discard);
                    }
                }
                return returnHTML;
            }

            getHTML() {
                var returnHTML = '<div id="factory'+this.id+'" class="factory tileContainer" factoryId="'+this.id+'">';
                if(this.isCenter) {
                    returnHTML = '<div id="center" class="tileContainer" factoryId="0">';
                }
                returnHTML += this.updateHTML();

                return returnHTML+'</div>';
            }

            take(colour) {
                if(this.id != 0) {
                    for(var tile of this.tiles) {
                        if(tile.colour != colour) {
                            factories[0].tiles.push(tile);
                        }       
                    }
                    this.tiles = [];
                    $('#factory'+this.id+' .tile.'+colour).remove();
                    $('#center').append($('#factory'+this.id).find('.tile'));
                }
                else {
                    $('#center .tile.'+colour).remove();
                    for (var i = this.tiles.length -1; i >= 0; i--) {
                        if(this.tiles[i].colour == colour) {
                            this.tiles.splice(i,1);
                        }
                    }
                }
                endRound();
            }        
        }

        class Tile {
            constructor(colour,where) {
                this.id = allTiles.length;
                this.colour = colour;
                if(!this.colour) {
                    this.colour = BLACK;
                }
                if(where == DISCARD) {
                    discard.push(this);
                }
                else {
                    if(where == CENTER) {
                        factories[0].tiles.push(this);
                        factories[0].update();
                    }
                    else {
                        bag.push(this);
                    }
                }
                allTiles.push(this);
            }
        }

        class Player {
            constructor(cpu) {
                this.cpu = cpu;
                this.id = players.length;
                this.score = 0;
                this.board = new Board(this);
                this.name = 'Computer '+(this.id);
                if(this.id == 0) {
                    this.name = playerName;
                }
                players.push(this);
            }

            boardHTML() {
                return this.board.boardHTML(this.name + '<br/>'+'<span class="score">'+this.score+'</span>');
            }

            addScore(scored) {
                this.score += scored;
                $('#player'+this.id+' .score').html(this.score);
                if(scored < 0) {
                    this.board.broken += scored;                    
                }
            }

            doTurn() {
                $('#game').addClass('computer');
                var boardClone = new Board(this);
                boardClone.wall = cloneObject(this.board.wall); //wall is basically where tiles are moved for scoring
                boardClone.lines = cloneObject(this.board.lines); // lines are where tiles are scored on each turn
                boardClone.overflow = cloneObject(this.board.overflow); // the bottom line
                boardClone.dummyBoard = true; //some stuff needed for this particular implementation
                boardClone.endRound(true);            

                var linesDemand = [ //objects used to calculate best move for each color
                    {"line":-1,"colour":RED,"score":-100,"amount":1},
                    {"line":-1,"colour":WHITE,"score":-100,"amount":1},
                    {"line":-1,"colour":BLUE,"score":-100,"amount":1},
                    {"line":-1,"colour":YELLOW,"score":-100,"amount":1},
                    {"line":-1,"colour":BLACK,"score":-100,"amount":1}
                ];
                for(var y=0; y<5; y++) { // for each line on the board
                    var thisLine = boardClone.lines[y]; 
                    if(thisLine[thisLine.length - 1] == null) { // if the row is not full
                        for(var col of colours) { // for each color
                            var missing = 0;
                            for(var t=0; t<thisLine.length; t++) { // count how many tiles are missing
                                if(thisLine[t] == null) {          // imo, this should be done before the color loop
                                    missing++;
                                }
                            }
                            for(var x=0; x<5; x++) { // for each tile on the corresponding row on the wall         
                                if((thisLine[0] == col || thisLine[0] == null) && // if (wall's color spot is empty and
                                                                                  //     wall's color spot matches col)
                                   (!boardClone.wall[y][x].filled && boardClone.wall[y][x].colour == col)) { 
                                    // add demand of current color with line, score (including bonus), and amount of missing tiles in the line 
                                    var demand = {"line":y,"colour":col,"score":boardClone.calcScorePlacedTile(y,x),"amount":missing};
                                    linesDemand.push(demand);
                                }
                            }                        
                        }
                    }
                }   
                var possiblesTileSets = [];
                var highestScore = -1000;
                var bestOption;
                for(var fac of factories) { // for each factory
                    var facOptions = {};  // list of options for the each color
                    facOptions[RED]=0;
                    facOptions[WHITE]=0;
                    facOptions[BLUE]=0;
                    facOptions[YELLOW]=0;
                    facOptions[BLACK]=0;
                    for(var tile of fac.tiles) { // count how many of each color we have in the current fac
                        facOptions[tile.colour]++;
                    }
                    for(var facOpt in facOptions) { // for each color in the factory
                        if(facOptions.hasOwnProperty(facOpt) && facOptions[facOpt] > 0 ) {
                            var bestScore = -1000;
                            var bestLine;
                            for(var demand of linesDemand) { // for each demand with the same color as facOpt
                                var score = -200;
                                if(demand.colour == facOpt) {
                                    score = demand.score;  // let score be the score achieved by choosing demand 
                                    if(fac.id == 0 && !firstTokenTaken) { // if first token is not taken and factory is the middle one
                                        score--;
                                    }
                                    if(demand.amount >= facOptions[facOpt]) { // if factory has tiles less than the empty spaces
                                        score += (facOptions[facOpt]*1.1)+(demand.amount*0.1); // add score based on that
                                        if(demand.amount == facOptions[facOpt]) { // if it's a perfect fit, add one more
                                            score++;
                                        }
                                    }
                                    else { // deduct overflow points to the score
                                        for(var over=0; over < facOptions[facOpt]-demand.amount; over++) {
                                            var overflowNumber = boardClone.overflow.length + over;
                                            if(overflowNumber < overflowPoints.length) {
                                                score += overflowPoints[overflowNumber];
                                            }
                                        }                                    
                                    }
                                }
                                if(score > bestScore) { // update the best demand for the current factory
                                    bestScore = score;
                                    bestLine = demand.line;
                                }
                            }

                            // remember the analysis info
                            var possibleTileSet = {"factory":fac.id,"colour":facOpt,"amount":facOptions[facOpt],"score":bestScore,"line":bestLine};
                            if(bestScore > highestScore) { //if bestScore is better than the highest score over all factories
                                highestScore = bestScore;
                                bestOption = possibleTileSet; // remember it
                            }
                            possiblesTileSets.push(possibleTileSet);
                        }
                    }
                }
                if(bestOption) { // make the move
                    var self = this;                                    
                    var fromFactory = $('#factory'+bestOption.factory);
                    if(bestOption.factory == 0) {
                        fromFactory = $('#center');
                    }
                    var beginPos = fromFactory.offset();
                    var toLine = $('#player'+self.id+' .boardLine'+bestOption.line);
                    if(bestOption.line < 0) {
                        toLine = $('#player'+self.id+' .overflowLine');
                    }   
                    var endPos = toLine.offset();

                    var dragTiles = fromFactory.find('.tile[tilecolour="'+bestOption.colour+'"],[tilecolour="'+FIRST+'"]');                   
                    var tileDragger = $('<div class="tileDragger"></div>').append(dragTiles);         
                    $(tileDragger).css({"z-index":zIndexCounter,"left":beginPos.left+fromFactory.width()/2,"top":beginPos.top+fromFactory.height()/2});
                    zIndexCounter++;
                    $('#game').append(tileDragger);
                    $(tileDragger).animate({"left":endPos.left+toLine.width()*0.8,"top":endPos.top},cpuSpeed,function(){
                        if(bestOption.factory == 0 && factories[bestOption.factory].tiles.findIndex(f => f.colour == FIRST) != -1) {
                            self.addScore(-1);
                        }
                        if(bestOption.line > -1) {                            
                            self.board.addTiles(bestOption.line, bestOption.colour, bestOption.amount , bestOption.factory);
                        }
                        else {
                            self.board.overflowTiles(bestOption.colour, bestOption.amount , bestOption.factory);
                        }
                        $(tileDragger).remove();
                        $('#game').removeClass('computer'); 
                    });                                                
                }
                else {
//                    console.log('ERROR: No options to pick !');
//                    console.log('options',possiblesTileSets);
//                    console.log('demand',linesDemand);
//                    console.log('factories',factories);
//                    console.log('players',players);
//                    console.log('bag',bag);
//                    console.log('discard',discard);
                    $('#game').removeClass('computer');
                }
            }
        }

        class Board {
            constructor(player) {
                this.player = player;
                this.id = player.id;
                this.wall = [];
                for(var y=0; y<5; y++) {
                    this.wall[y] = [];
                    for(var x=0; x<5; x++) {
                        this.wall[y][x] = {
                            "filled":false,
                            "colour":pattern[y][x]
                        }
                    }
                }
                this.lines = [];
                for(var x=0; x<5; x++) {
                    this.lines[x] = [];
                    for(var t=0; t<=x; t++) {
                        this.lines[x][t] = null;
                    }
                }
                this.overflow = [];
                this.dummyBoard = false;
                
                this.broken = 0;
                this.roundScoreInfo = ['<u>Round 1</u>'];
                this.gameScoreInfo = ['<u>Round 1</u>'];
            }

            boardHTML(playerInfo) {
                var linesHTML = '<div class="boardLines">';
                var wallHTML = '<div class="boardWall">';
                var overflowHTML = '<div class="overflowLine">';
                for(const[indexY,y] of this.lines.entries()) {
                    linesHTML += '<div class="boardLine boardLine'+indexY+'" boardLine="'+indexY+'">';
                    for(const[indexX,x] of y.entries()) {
                        var colourClass = '';
                        if(x != null) {
                            colourClass = 'tileFilled_true '+x.colour;
                        } 
                        linesHTML += '<div class="tile lineTile lineTile'+indexX+' '+colourClass+'" boardTile="'+indexX+'"></div>';
                    }
                    linesHTML += '</div>';
                }
                linesHTML += '</div>';
                for(const[indexY,y] of this.wall.entries()) {
                    wallHTML += '<div class="wallLine wallLine'+indexY+'" wallLine="'+indexY+'">';
                    for(const[indexX,x] of y.entries()) {
                        var colourClass = x.colour;
                        colourClass+= ' tileFilled_'+x.filled;
                        wallHTML += '<div class="tile lineTile lineTile'+indexX+' '+colourClass+'" wallTile="'+indexX+'"></div>';
                    }
                    wallHTML += '</div>';
                }
                wallHTML += '</div>';


                for(var f=0; f < 7; f++) {
                    var colourClass = '';
                    if(this.overflow[f]) {
                        colourClass = this.overflow[f];
                    }
                    overflowHTML += '<div class="overflow overflow'+f+'"><div class="overflowPoints">'+overflowPoints[f]+'</div><div class="tile lineTile '+colourClass+'" overflow="'+f+'"></div></div>'
                }
                overflowHTML += '<div class="playerInfo">'+playerInfo+'<div class="scoringInfo">Nothing scored yet</div></div>';
                overflowHTML += '</div>'
                return '<div class="board">'+linesHTML+wallHTML+overflowHTML+'</div>';
            }

            addTiles(boardLine, colour, amount, factoryId) {
                if(this.checkFirstPlayerTile(factoryId) && !this.player.cpu) {
                    this.player.addScore(overflowPoints[this.overflow.length]);
                }
                for(var x=0; x < amount; x++) {                
                    var slotFound = false;
                    for(const[indexTile,slot] of this.lines[boardLine].entries()) {
                        if(slot == null) {
                            this.lines[boardLine][indexTile] = colour;
                            slotFound = true;
                            break;
                        }
                    }
                    if(!slotFound) {
                        if(this.overflow.length < 7) {
                            this.player.addScore(overflowPoints[this.overflow.length]);
                            this.overflow.push(colour);    
                            if(cpuSpeed >= 500 || this.id == 0) {
                                setTimeout(function(){
                                    sound.play("breakTile");
                                },200*x);      
                            }
                        }
                        else {
                            new Tile(colour,DISCARD);
                        }
                    }
                    if(cpuSpeed >= 500 || this.id == 0) {
                        setTimeout(function(){
                            sound.play("gatherTiles");
                        },200*x);      
                    }
                }
                factories[factoryId].take(colour);
                this.updateTiles();                 
            }

            overflowTiles(colour, amount, factoryId) {
                if(this.checkFirstPlayerTile(factoryId)) {
                    amount--;
                }                
                for(var x=0; x < amount; x++) {
                    if(this.overflow.length < 7) {
                        this.player.addScore(overflowPoints[this.overflow.length]);
                        this.overflow.push(colour);
                        if(cpuSpeed >= 500 || this.id == 0) {
                            setTimeout(function(){
                                sound.play("breakTile");
                            },200*x);      
                        }
                    }
                    else {
                        new Tile(colour,DISCARD);
                    }
                }
                factories[factoryId].take(colour);
                this.updateTiles();
            }

            updateTiles() {
                for(const[indexLine,line] of this.lines.entries()) {
                    for(const[indexTile,tile] of line.entries()) {
                        if(tile) {
                            $('#player'+this.id+' .boardLine'+indexLine+' .lineTile'+indexTile).addClass('tileFilled_true').addClass(tile);
                        }
                        else {
                            $('#player'+this.id+' .boardLine'+indexLine+' .lineTile'+indexTile).removeClass('tileFilled_true').removeClass('blue yellow rojo white black first');
                        }
                    }
                }
                for(const[indexWallLine,wallLine] of this.wall.entries()) {               
                    for(const[indexTile,tile] of wallLine.entries()) {                    
                        if(tile.filled) {
                            $('#player'+this.id+' .wallLine'+indexWallLine+' .lineTile'+indexTile).addClass('tileFilled_true').removeClass('tileFilled_false');
                        }
                    }
                }

                for(var f=0; f < 7; f++) {
                    if(this.overflow[f]) {
                        $('#player'+this.id+' .overflow'+f+' .tile').addClass('tileFilled_true').addClass(this.overflow[f]);                    
                    }
                    else {
                        $('#player'+this.id+' .overflow'+f+' .tile').removeClass('blue yellow rojo white black first tileFilled_true');
                    }
                }
            }

            endRound() {            
                if(!this.dummyBoard) {
                    // clear the overflow
                    for(var x=0; x < this.overflow.length; x++) {
                        if(this.overflow[x] != FIRST) {
                            new Tile(this.overflow[x], DISCARD);                
                        }
                    }
                    this.overflow = [];                      
                }
                 
                if(this.broken != 0) {
                    this.remember(this.broken+' : broken tiles');
                }
                for(const[indexLine,line] of this.lines.entries()) {
                    if(line.indexOf(null) == -1) {
                        var colour = line[0];
                        // add tile to wall
                        for(const[indexWallTile,wallTile] of this.wall[indexLine].entries()) {
                            if(wallTile.colour == colour) {                            
                                wallTile.filled = true;
                                if(!this.dummyBoard) {
                                    this.remember('{0} in {1} row', colour, indexLine);
                                    this.scorePlacedTile(indexLine,indexWallTile);
                                    // add removed tiles to discard
                                    // Wipe line
                                    for(var x=0; x < line.length; x++) {
                                        new Tile(colour, DISCARD);
                                        line[x] = null;
                                    }
                                }
                                break;
                            }
                        }
                    }
                }                
            }
            
            remember(scoreEvent, colour, row) {
                var rowNames = ['first','second','third','fourth','fifth'];
                if(typeof row != 'undefined') {
                    scoreEvent = scoreEvent.replace('{1}',rowNames[row]);
                }
                if(typeof colour != 'undefined') {
                    scoreEvent = scoreEvent.replace('{0}','<span class="tile '+colour+' tinyTile"></span>');
                }
                this.roundScoreInfo.push(scoreEvent);
                this.gameScoreInfo.push(scoreEvent);                
            }
            
            writeScoreInfoToBoard(type) {
                var scoring = '';
                if(type == 'round') {
                    for(var s of this.roundScoreInfo) {
                        scoring += s +'<br/>';
                    }
                }
                if(type == 'game') {
                    for(var s of this.gameScoreInfo) {
                        scoring += s +'<br/>';
                    }
                }
                
                if(scoring == '') {
                    scoring = 'Nothing scored yet';
                }                
                
                $('#player'+this.player.id+' .scoringInfo').html(scoring);
            }
            

            visualiseEndRound() {
                var self = this;                
                for(const[indexLine,line] of this.lines.entries()) {
                    if(line.indexOf(null) == -1) {
                        var colour = line[0];
                        // add tile to wall
                        for(const[indexWallTile,wallTile] of this.wall[indexLine].entries()) {
                            if(wallTile.colour == colour) {                            
                                $('#player'+self.player.id+' .boardLine'+indexLine+' .tileFilled_true').each(function(){
                                    var thisPos = $(this).offset();
                                    var toLoc = $('#player'+self.player.id+' .wallLine'+indexLine+' .'+colour);
                                    var toPos = toLoc.offset();
                                    $(this).removeClass('.tileFilled_true');
                                    var newDiv = $('<\div>').addClass('tile endRoundTile').addClass(colour).css({"left":thisPos.left,"top":thisPos.top});
                                    $('#game').append(newDiv);
                                    $(newDiv).animate({"top":toPos.top -toLoc.height()*0.75 ,"left":toPos.left},endRoundSpeed*0.75,function(){
                                        $(newDiv).animate({"top":toPos.top,"boxShadow":"0px 0px 0px rgba(0,0,0,1)"},endRoundSpeed*0.2,"easeInQuint",function(){
                                            if(!droppingTile) {
                                                sound.play("placeTile");
                                                droppingTile = true;
                                            }
                                            $(newDiv).fadeOut(endRoundSpeed,function(){
                                               $(newDiv).remove(); 
                                            });
                                        });
                                    });
                                });
                            }
                        }
                    }
                }
            }

            calcScorePlacedTile(y,x, rememberScoring) {
                var scored = 1;
                var self = this;
                var horizonalScoring = false;
                var verticalScoring = false;
                var scoredVertical = 0;
                var scoredHorizontal = 0;

                function checkNeighbours(neighbourY,neighbourX,direction) {
                    if(direction == 'up' && neighbourY > 0) {
                        if(self.wall[neighbourY-1][neighbourX].filled) {
                            scored++;
                            scoredVertical++;
                            checkNeighbours(neighbourY-1,neighbourX,direction);
							verticalScoring = true;
                        }
                    }
                    if(direction == 'down' && neighbourY < 4) {
                        if(self.wall[neighbourY+1][neighbourX].filled) {
                            scored++;
                            scoredVertical++;
                            checkNeighbours(neighbourY+1,neighbourX,direction);
							verticalScoring = true;
                        }
                    }

                    if(direction == 'left' && neighbourX > 0) {
                        if(self.wall[neighbourY][neighbourX-1].filled) {
                            scored++;
                            scoredHorizontal++;
                            checkNeighbours(neighbourY,neighbourX-1,direction);
							horizonalScoring = true;
                        }
                    }

                    if(direction == 'right' && neighbourX < 4) {
                        if(self.wall[neighbourY][neighbourX+1].filled) {
                            scored++;
                            scoredHorizontal++;
                            checkNeighbours(neighbourY,neighbourX+1,direction);
							horizonalScoring = true;
                        }
                    }
                }  

                function checkBonusPoints(y,x,colour) {
                    var addColourBonus = 0;            
                    var addVerticalBonus = 0;
                    var addHorizontalBonus = 0;

                    // check vertical bonus (7);
                    for(var checkY = 0; checkY < 5; checkY++) {
                        if(self.wall[checkY][x].filled) {
                            addVerticalBonus++;
                        }
                    }
                    // check horizontal bonus (2);
                    for(var checkX = 0; checkX < 5; checkX++) {
                        if(self.wall[y][checkX].filled) {
                            addHorizontalBonus++;                        
                        }
                         // also check colour bonus (10);
                        for(var checkY = 0; checkY < 5; checkY++) {
                            if(self.wall[checkY][checkX].colour == colour && self.wall[checkY][checkX].filled) {
                                addColourBonus++;
                            }
                        }
                    }

                    if(self.dummyBoard) {
                        scored += addColourBonus*0.2;
                        scored += addVerticalBonus*0.1;
                        scored += addHorizontalBonus*0.01;
                    }

                    if(addColourBonus == 5) {
                        scored += 10;
                        if(rememberScoring) {
                            self.remember('+10 bonus: all of one colour');
                        }
                    }
                    if(addVerticalBonus == 5) {
                        scored += 7;
                        if(rememberScoring) {
                            self.remember('+7 bonus: full column');
                        }
                    }
                    if(addHorizontalBonus == 5) {
                        scored += 2; 
                        if(rememberScoring) {
                            self.remember('+2 bonus: full row');
                        }
                        if(!self.dummyBoard) {
                            gameEnding = true;
                        }
                    }
                }

                checkNeighbours(y,x,'up');
                checkNeighbours(y,x,'down');
                checkNeighbours(y,x,'left');
                checkNeighbours(y,x,'right');
                
                if(rememberScoring && scoredVertical == 0 && scoredHorizontal == 0) {
                    this.remember('+1 : tile placed');
                }
                if(rememberScoring && scoredVertical > 0) {
                    this.remember('+'+(scoredVertical+1)+' : tiles connected vertically');
                }
                if(rememberScoring && scoredHorizontal > 0) {
                    this.remember('+'+(scoredHorizontal+1)+' : tiles connected horizontally');
                }
                checkBonusPoints(y,x,self.wall[y][x].colour);   
                if(horizonalScoring && verticalScoring) {
                        scored++;
                }
                return scored;
            }

            scorePlacedTile(y,x) {            
                this.player.addScore(this.calcScorePlacedTile(y,x, true));
            }

            filledRows() {
                var rowsFilled = 0;
                for(var wall of this.wall) {
                    var tilesFilled = 0;
                    for(var tile of wall) {
                        if(tile.filled) {
                            tilesFilled++;
                        }
                    }
                    if(tilesFilled == 5) {
                        rowsFilled++;
                    }
                }
                return rowsFilled;
            }

            canPlayColour(lineNumber,colour) {
                if(colours.indexOf(colour) != -1) {
                    var line = this.lines[lineNumber];
                    if(line[0] == null || (line[0] == colour && line[line.length] == null)) {
                        var wallLine = this.wall[lineNumber];
                        for(var x=0; x<5; x++) {
                            if(wallLine[x].colour == colour && wallLine[x].filled) {
                                return false;
                            }
                        }
                        return true;
                    }
                }
                return false;
            }

            checkFirstPlayerTile(factoryId) {
                if(factoryId == 0 && !firstTokenTaken) {
                    firstTokenTaken = true;
                    firstPlayer = this.id;
                    this.overflow.push(FIRST);
                    factories[0].tiles.splice(0,1);
                    $('#center .tile.first').remove();
                    return true;
                }
            }

        }        

        var cloneObject = (myObject) => {
                return $.parseJSON(JSON.stringify(myObject));
        };

        var endRound = () => {
            activePlayer++;
            if(activePlayer >= players.length) {
                activePlayer = 0;            
            }

            var startNewRound = true;
            for(var fac of factories) {
                if(fac.tiles.length > 0) {
                    startNewRound = false;
                }
            }

            $('.activePlayer').removeClass('activePlayer');

            if(startNewRound) {
                activePlayer = firstPlayer;  
                roundNumber++;
                for(var p=0; p < players.length; p++) {                
                    players[p].board.visualiseEndRound(endRoundSpeed); 
                }
                setTimeout(function(){    
                    droppingTile = false;
                    for(var p=0; p < players.length; p++) {                         
                        players[p].board.endRound();
                        players[p].board.updateTiles();
                        players[p].board.writeScoreInfoToBoard('round');                        
                    }
                    if(!gameEnding) {
                        for(var x=1; x < factories.length; x++) {
                            factories[x].refillAndUpdate();
                        }
                        new Tile(FIRST,CENTER);
                        firstTokenTaken = false;
                        for(var p=0; p < players.length; p++) { 
                            players[p].board.roundScoreInfo = [];
                            players[p].board.broken = 0;
                            players[p].board.remember('<u>Round '+(roundNumber+1)+'</u>');
                        }
                    }

                    if(gameEnding) {
                        gameover();                    
                    }
                    else {
                        $('#player'+activePlayer).addClass('activePlayer');
                        if(players[activePlayer].cpu) {
                            players[activePlayer].doTurn();
                        }
                    }
                },endRoundSpeed);   
            }
            else { 
                $('#player'+activePlayer).addClass('activePlayer');
                if(players[activePlayer].cpu) {
                    players[activePlayer].doTurn();
                }
            }
        }

        var gameover = () => {
            sound.play("gameover");
            $('.dragInitialised').draggable('destroy');
            var data = {
              "name":  playerName,
              "players": playerCount,
              "score" : players[0].score
            };
            $.post("server.php", data);
            activePlayer = -1;

            var winner = 0;
            var highest = -1000;
            for(var p of players) {           
                if(p.score > highest) {
                    highest = p.score;
                    winner = p;
                }
                if(p.score == highest) {
                    if(p.board.filledRows() > winner.board.filledRows()) {
                        highest = p.score;
                        winner = p;
                    }                
                }
                p.board.remember('Game Over');
                p.board.writeScoreInfoToBoard('game');
            }
            var winText = 'You have won !<div class="smallGameover">Score: '+winner.score+' points</div>';
            if(winner.cpu) {
               winText = 'Defeat<div class="smallGameover">'+winner.name+': '+winner.score+' points</div>';
            }
            $('#game').append('<div id="gameover">'+winText+'</div>');
            $('#gameover').append('<div id="again"><button class="playagain">Play again</button></div>');
            $('.playagain').on('click',function(){
                location.reload();
                $(this).off();
                $('#game').fadeOut();
            });
            $('#gameover').draggable();
            
        }

        var shuffle = (myList) => {
            var j, x, i;
            for (i = myList.length; i; i -= 1) {
                j = Math.floor(Math.random() * i);
                x = myList[i - 1];
                myList[i - 1] = myList[j];
                myList[j] = x;
            }    
            return myList;
        }

        var makeTilesDraggable = () => {        
            $('.tileContainer .tile:not(.dragInitialised)').draggable({
                containment:$('#game'),
                cursorAt: { left: 50, top :50 },
                helper: function() {
                    fromFactory = $(this).parent();
                    var thisPos = $(this).position();
                    dragColour = $(this).attr('tilecolour');                
                    var dragTiles = fromFactory.find('.tile[tilecolour="'+dragColour+'"],[tilecolour="'+FIRST+'"]');
                    dragging = dragTiles;
                    var tileDragger = $('<div class="tileDragger"></div>').append(dragTiles);         
                    $(tileDragger).css({"z-index":zIndexCounter,"left":thisPos.left,"top":thisPos.top});
                    zIndexCounter++;
                    $('#game').append(tileDragger);
                    return tileDragger;
                },
                revert : function() {
                    if ($(this).hasClass('no-revert')) {
                        $(this).removeClass('no-revert');                                        
                    }
                    else {                    
                        $(fromFactory).append(dragging);
                        return true;
                    }
                },
            }).addClass('dragInitialised');
        }

        var initEvents = () => {
            $('.boardLine').droppable({
                accept: ".tileDragger, .tile",
                hoverClass: "dropHere",
                activate: function ( event, ui ) {
                    var playerBoard = parseInt($(this).parents('.player:first').attr('player'));
                    if(playerBoard == activePlayer) {
                        var boardLine = parseInt($(this).attr('boardline'));
                        if(players[playerBoard].board.canPlayColour(boardLine,dragColour)) {
                            $(this).addClass('dropHighlight');
                        }
                    }
                },
                drop: function( event, ui ) {
                    var playerBoard = parseInt($(this).parents('.player:first').attr('player'));                
                    if(playerBoard == activePlayer) {
                        var thisPlayer = players[playerBoard];
                        var boardLine = parseInt($(this).attr('boardline'));
                        if(thisPlayer.board.canPlayColour(boardLine,dragColour)) {
                            ui.draggable.addClass('no-revert');
                            var factoryId = parseInt($(fromFactory).attr('factoryId'));
                            var dragAmount = dragging.length;
                            dragging.each(function(){
                                if($(this).hasClass(FIRST)) {
                                    dragAmount--;
                                }
                            })
                            thisPlayer.board.addTiles(boardLine, dragColour, dragAmount , factoryId);
                        }
                    }
                },
                deactivate: function ( event, ui ) {
                    $(this).removeClass('dropHighlight');
                }
            });

            $('.overflowLine').droppable({
                accept: ".tileDragger, .tile",
                hoverClass: "dropHere",
                activate: function ( event, ui ) {
                    var playerBoard = parseInt($(this).parents('.player:first').attr('player'));
                    var thisPlayer = players[playerBoard];
                    if(playerBoard == activePlayer) {
                        $(this).addClass('dropHighlight');
                    }                
                },
                drop: function( event, ui ) {
                    var playerBoard = parseInt($(this).parents('.player:first').attr('player'));
                    var boardLine = parseInt($(this).attr('boardline'));
                    var thisPlayer = players[playerBoard];
                    if(playerBoard == activePlayer) {   
                        ui.draggable.addClass('no-revert');
                        var factoryId = parseInt($(fromFactory).attr('factoryId'))
                        thisPlayer.board.overflowTiles(dragColour, dragging.length, factoryId);                    
                    }
                },
                deactivate: function ( event, ui ) {
                    $(this).removeClass('dropHighlight');
                }
            });   
            
            if(isMobileDevice()) {
                $('.playerInfo').on('mousedown',function(){
                    $(this).find('.scoringInfo').toggle();
                });
            }
        }

        var init = (playerCount) => {                       
            // init Tiles
            for(var c of colours) {
                for(var x=0; x < 20; x++) {
                    new Tile(c);
                }
            }
            bag = shuffle(bag);

            // init Factories
            var factoryCount = [0,0,5,7,9];
            for(var x=0; x < factoryCount[playerCount]+1; x++) {
                var newFactory = new Factory();
                if(x > 0) {
                    $('#factories').append(newFactory.getHTML());
                }
            }
            makeTilesDraggable();

            // init Player
            for(var x=0; x < playerCount; x++) {
                var cpu = true;
                if(x==0) {
                    cpu = false;
                }
                var newPlayer = new Player(cpu);    
                var newPlayerHTML = '<div id="player'+x+'" class="player" player="'+x+'">'+newPlayer.boardHTML()+'</div>';
                if(x ==0 || x == 2) {
                    $('#playersLeft').append(newPlayerHTML);
                }
                else {
                    $('#playersRight').append(newPlayerHTML);
                }
            } 
            $('#player'+activePlayer).addClass('activePlayer');

            // add first player marker
            new Tile(FIRST,CENTER);
            
            sound = new Howl({
                src: ['rojo.mp3'], 
                sprite: {
                    placeTile: [0, 200],
                    gatherTiles: [500, 200],
                    gameover: [1000, 1000],
                    breakTile : [2000,300]
                }
            });            
        }    

        // randomize start player
        var start = () => {
            firstPlayer = Math.floor(Math.random()*players.length);
            activePlayer = firstPlayer;
            $('#player'+activePlayer).addClass('activePlayer');
            if(players[activePlayer].cpu) {
                    players[activePlayer].doTurn();
            }
        }
        
        init(playerCount);
        initEvents();
        sound.volume(volume);  
        start();
    }
    
    var useLocalStorage = (varName, value, get, parseValue) => {
        var hasLocalStorage;
        try {
            hasLocalStorage = 'localStorage' in window && window['localStorage'] !== null;
         }
        catch (e) {
            return value;
        }
        var returnValue = value;
        if(hasLocalStorage) {
            if(get) {
                var returnValue = localStorage[varName];
                if(typeof returnValue != "undefined") {
                    try {
                        returnValue = JSON.parse(returnValue);
                    }
                    catch(e2) {}
                }
                else {
                    returnValue = value;
                }
            }
            else {
                localStorage[varName] = value;
            }
        }
        return returnValue
    }
    
    $('#playerName').val(useLocalStorage("rojo_name","Anonymous",true));
    $('#volume').val(useLocalStorage("rojo_sound","1",true));
    $('#playerCount').val(useLocalStorage("rojo_players","4",true));
    $('#cpuSpeed').val(useLocalStorage("rojo_speed","1000",true));
    $('.play').on('click',function(){
        $(this).off();
        playerCount = parseInt($('#playerCount').val());
        cpuSpeed = parseInt($('#cpuSpeed').val());
        volume = parseInt($('#volume').val());
        playerName = $('#playerName').val().substring(0,12);
        useLocalStorage("rojo_name",playerName);
        useLocalStorage("rojo_sound",volume);
        useLocalStorage("rojo_players",playerCount);
        useLocalStorage("rojo_speed",cpuSpeed);
        endRoundSpeed = cpuSpeed;
        $('#game').removeClass('intro');
        rojo();
    });
    $('#highscores').on('click mousedown',function(){
        $('#scoreModal').fadeIn();
        $.get("server.php",function(data){
            var parsedData = JSON.parse(data);
            for(var playerAmount=4; playerAmount > 1; playerAmount--) {
                highHTML = '<div class="highRank">Rank</div><div class="highName">Name</div><div class="highScore">Score</div>';
                var playersData = parsedData[playerAmount];
                if(playersData) {
                    for(const[index, playerHigh] of playersData.entries()) {
                        highHTML += '<div class="highRank high'+index+'">'+(index+1)+'</div>';
                        highHTML += '<div class="highName high'+index+'">'+playerHigh.name+'</div>';
                        highHTML += '<div class="highScore high'+index+'">'+playerHigh.score+'</div>';
                    }
                    $('#scoreModal #scoreMode'+playerAmount).html(highHTML);
                }
            }
        });
    });
    $('#close').on('click mousedown',function() {
        $('#scoreModal').fadeOut();
    });
    
    var isMobileDevice = () => {
        return (typeof window.orientation !== "undefined") || (navigator.userAgent.indexOf('IEMobile') !== -1);
    };
    
    var url = document.location+'';
    if( url.indexOf('basic') != -1) {
        $('#game').addClass('basic');
    }
}); 

