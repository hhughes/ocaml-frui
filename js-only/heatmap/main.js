var main_elt = $("div#main");
var elements = {};

var time = 0;
var inc = 3600000;

var elec_data = {};

var rooms = [];

var min_date = 0;
var max_date = 2000;

function pos(r) {
    switch(r) {
    case "GN17":
	return { left: 470, top: 90 };
    case "FN30":
	return { left: 1120, top: 70 };
    case "SN30":
	return { left: 1730, top: 70 };
    default:
	return { left: 0, top: 0};
    }
}

function load_objects(ht_d, o) {
    var e = eval(o)[0];
    var room = e.room;
    var data = e.data;
    for(x in data) {
	var d = data[x];
	var y = parseInt(d[0]);
	var v = parseInt(d[1]);
	if(ht_d[y] == null) {
	    ht_d[y] = [];
	}
	var x = {
	    "room": room,
	    "value": v
	};
	ht_d[y].push(x);
	if(!ht_d.max || v > ht_d.max) {
	    ht_d.max = v;
	}

	if(y > max_date) {
	    max_date = y;
	}

	if(!min_date || y < min_date) {
	    min_date = y;
	}
    }
}

function create_elements() {
    for(r in rooms) {
	var room = rooms[r];
	var div = $(document.createElement("div")).addClass("room").attr("title", room);
	main_elt.append(div);
	elements[room] = div;
	div.offset(pos(room));
    }
}

function render(r) {
    var rs = {};
    
    for(a in r[time]) {
	var d = r[time][a];
	if(a != "max") {
	    rs[d.room] = d.value;
	}
    }

    for(x in rooms) {
	var room = rooms[x];
	var p = Math.round((rs[room] * 255) / r.max);
	elements[room].css({ backgroundColor: "rgb(" + p + "," + (255 - p) + ",0)" });
    }
}

function set_time(y) {
    time = y;
    $("div#spinner > input").val(time);
}

function load() {
    var loaded = 3;
    function all_done() {
	loaded--;
	if(!loaded) {
	    set_time(min_date);
	    for(r in elec_data[max_date]) {
		room = elec_data[max_date][r].room;
		rooms.push(room);
	    }
	    create_elements();
	    render(elec_data);
	}
    }

    function load_elec(o) {
	load_objects(elec_data, o);
	all_done();
    }

    $.get("http://localhost:8080/elec", load_elec);
    $.get("http://localhost:8080/elec2", load_elec);
    $.get("http://localhost:8080/elec3", load_elec);
}

function create_spinner() {

    function spinner_up() {
	time+=inc;
	$("div#spinner > input").val(time);
	render(elec_data);
    }
    
    function spinner_down() {
	time-=inc;
	$("div#spinner > input").val(time);
	render(elec_data);
    }

    var up = $(document.createElement("button")).addClass("up").text("^");
    var down = $(document.createElement("button")).addClass("down").text("v");
    var input = $(document.createElement("input")).addClass("spinner");

    up.click(spinner_up);
    down.click(spinner_down);

    var spinner = $("div#spinner");
    spinner.append(up);
    spinner.append(down);
    spinner.append(input);
}

create_spinner();

$("button#load").click(load);
