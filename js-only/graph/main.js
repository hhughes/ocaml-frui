var main_elt = $("div#main");
var elements = {};

var time = 0;

var gdp_data = {};

var life_data = {};

var ids = [];

var min_year = 2000;
var max_year = 2000;

function load_objects(ht_d, o) {
    var e = eval(o);
    var data = e[1];
    for(x in data) {
	var d = data[x];
	var v = parseInt(d.value);
	var y = parseInt(d.date);
	if(ht_d[y] == null) {
	    ht_d[y] = [];
	}
	var x = {
	    "id": d.country.id,
	    "value": v
	};
	ht_d[y].push(x);
	if(!ht_d.max || v > ht_d.max) {
	    ht_d.max = v;
	}

	if(y > max_year) {
	    max_year = y;
	}

	if(y < min_year) {
	    min_year = y;
	}
    }
}

function create_elements() {
    for(i in ids) {
	var id = ids[i];
	var div = $(document.createElement("div")).addClass("datum").attr("title", id);
	main_elt.append(div);
	elements[id] = div;
    }
}

function render(x, y) {
    var xs = {};
    var ys = {};
    
    for(a in x[time]) {
	var d = x[time][a];
	if(a != "max") {
	    xs[d.id] = d.value;
	}
    }

    for(b in y[time]) {
	var d = y[time][b];
	if(a != "max") {
	    ys[d.id] = d.value;
	}
    }

    var w = main_elt.width();
    var h = main_elt.height();

    for(i in ids) {
	var id = ids[i];
	elements[id].offset({
		left: (xs[id] * w) / x.max,
		    top: h - ((ys[id] * h) / y.max)
		    });
    }
}

function set_time(y) {
    time = y;
    $("div#spinner > input").val(time);
}

function load() {
    var n = parseInt($("input#n").val()) + 1;
    var loaded = n * 2;
    function all_done() {
	loaded--;
	if(!loaded) {
	    set_time(2009);
	    for(i in gdp_data[2009]) {
		id = gdp_data[2009][i].id;
		ids.push(id);
	    }
	    create_elements();
	    render(gdp_data, life_data);
	}
    }

    function load_gdp(o) {
	load_objects(gdp_data, o);
	all_done();
    }

    function load_life(o) {
	load_objects(life_data, o);
	all_done();
    }
    for(var i=1; i<=n; i++) {
	$.get("http://localhost:8080/gdp/gdp-" + i + ".json", load_gdp);
	$.get("http://localhost:8080/life/life-" + i + ".json", load_life);
    }
}

function create_spinner() {

    function spinner_up() {
	time++;
	$("div#spinner > input").val(time);
	render(gdp_data, life_data);
    }
    
    function spinner_down() {
	time--;
	$("div#spinner > input").val(time);
	render(gdp_data, life_data);
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
