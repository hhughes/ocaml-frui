var visualiser_elt = $("div#visualiser");
var timeline_elt = $("div#timeline");
var pie_elt = $("div#pie");
var cloud_elt = $("div#cloud");
var messages = {};
var functions = {};
var threads = {};

var pie_pieces = {};
var total_msgs = 0;

var cloud_words = {};
var total_words = 0;

var t_min = 0;
var t_max = 0;

function layout_div(msg) {
    var parent = $(msg.div).parent();
    var m_width = visualiser_elt.width();
    var t_width = t_max - t_min;
    var left = 0;
    var width = 0;
    var top = 0;
    var height = 20;
    var t_left = 0;
    if(msg.ty == "fn" || msg.ty == "msg") {
	var thread = threads[msg.tid];
	t_left = msg.ts - thread.ts;
	left = (t_left * m_width) / t_width;
	
	if(msg.ty == "fn") {
	    var finish = (msg.finish || t_max) - thread.ts;
	    width = ((finish - t_left) * m_width) / t_width;
	} else {
	    width = 2;
	}

    } else {
	var finish = (msg.finish || t_max) - t_min;
	t_left = msg.ts - t_min;
	left = (t_left * m_width) / t_width;
	width = ((finish - t_left) * m_width) / t_width;
	top = msg.tid * 24;
    }

    msg.div.offset({left: left + parent.offset().left, top: top + parent.offset().top});
    msg.div.width(width);
    msg.div.height(height);
}

function _layout_all(ht) {
    for(m in ht) {
	var msg = ht[m];
	if(msg != null && (typeof msg) != "function") {
	    layout_div(msg);
	}
    }
}

function layout_all() {
    _layout_all(threads);
    _layout_all(functions);
    _layout_all(messages);
}

function create_div(parent, msg) {
    var div = $(document.createElement("div")).addClass(msg.ty).attr("title", msg.desc);
    div.appendTo(parent);
    return div;
}

function set_t(t) {
    var layout = false;
    if(t_min == 0 || t < t_min) {
	t_min = t;
	$("div#min > input").val(t_min);
	layout = true;
    }

    if(t > t_max) {
	t_max = t;
	$("div#max > input").val(t_max);
	layout = true;
    }

    if(layout) {
	layout_all();
    }
}

function draw_slice(ctx, r, s) {
    ctx.beginPath();
    ctx.arc(60, 60, 50, r, r+s, false);
    ctx.lineTo(60,60);
    ctx.closePath();
    ctx.stroke();
    return r+s;
}

function draw_pie() {
    var canvas = pie_elt.find("canvas");
    var ctx = canvas[0].getContext("2d");

    ctx.clearRect(0, 0, 200, 200);

    ctx.strokeStyle = "black";
    ctx.beginPath();
    ctx.arc(60, 60, 55, 0, Math.PI * 2);
    ctx.closePath();
    ctx.stroke();
    
    ctx.strokeStyle = "red";
    var r = 0;
    for(p in pie_pieces) {
	var count = pie_pieces[p];
	if(count != null && (typeof count) != "function") {
	    r = draw_slice(ctx, r, (count * 2 * Math.PI) / total_msgs);
	}
    }
}

function add_to_pie(ty) {
    if(!pie_pieces[ty]) {
	pie_pieces[ty] = 0;
    }

    pie_pieces[ty]++;
    draw_pie();
}

function draw_cloud() {
    var canvas = cloud_elt.find("canvas");
    var ctx = canvas[0].getContext("2d");

    ctx.clearRect(0, 0, 200, 200);

    ctx.strokeStyle = "red";
    var r = 50;
    for(c in cloud_words) {
	var count = cloud_words[c];
	if(count != null && (typeof count) != "function") {
	    var height = Math.floor((count * 100) / total_words);
	    ctx.font = height + "px serif";
	    ctx.strokeText(c, 0, r);
	    r+=height;
	}
    }

}

function _add_to_cloud(w) {
    total_words++;
    if(!cloud_words[w]) {
	cloud_words[w] = 0;
    }

    cloud_words[w]++;
    draw_cloud();
}

function add_to_cloud(msg) {
    w_name = msg.name.split(" ");
    w_desc = msg.desc.split(" ");
    for(w in w_name) {
	var word = w_name[w];
	if(word != null && (typeof word) != "function") {
	    _add_to_cloud(word);
	}
    }

    for(w in w_desc) {
	var word = w_desc[w];
	if(word != null && (typeof word) != "function") {
	    _add_to_cloud(word);
	}
    }

}

function load_msg_obj(o) {
    var msgs = eval(o);
    for(m in msgs) {
	var msg = msgs[m];
	total_msgs++;
	set_t(msg.ts);
	add_to_pie(msg.ty);
	add_to_cloud(msg);
	switch(msg.ty) {
	    case "t_start":
		//create t
		msg.ty = "thread";
		msg.div = create_div(visualiser_elt, msg)
	        threads[msg.tid] = msg
		layout_div(msg);
		break;
	    case "t_finish":
		//close t
		threads[msg.tid].finish = msg.tid;
		layout_div(threads[msg.tid]);
		break;
	    case "fn_start":
		//create fn
		msg.ty = "fn";
		msg.div = create_div(threads[msg.tid].div, msg)
	        functions[msg.tid + msg.name] = msg
		layout_div(msg);
		break;
	    case "fn_finish":
		//close fn
		var fn = functions[msg.tid + msg.name];
		fn.finish = msg.ts;
		layout_div(fn);
		break;
	    case "msg":
		//create msg
		msg.div = create_div(threads[msg.tid].div, msg)
	        functions[msg.ts] = msg
		layout_div(msg);
		break;
		}
    }
}

function next_msg() {
    $.get("http://localhost:8080/next_msg", load_msg_obj)
}

function start() {
    setInterval(next_msg, 500);
}

function create_spinner(id, f_up, f_down) {
    var spinner = $(document.createElement("div")).attr("id", id);
    var up = $(document.createElement("button")).addClass("up").text("^");
    var down = $(document.createElement("button")).addClass("down").text("v");
    var input = $(document.createElement("input")).addClass("spinner");

    up.click(f_up);
    down.click(f_down);

    spinner.append(up);
    spinner.append(down);
    spinner.append(input);
    timeline_elt.append(spinner);
}

function min_up() {
    t_min++;
    $("div#min > input").val(t_min);
    layout_all();
}

function min_down() {
    t_min--;
    $("div#min > input").val(t_min);
    layout_all();
}

function max_up() {
    t_max++;
    $("div#max > input").val(t_max);
    layout_all();
}

function max_down() {
    t_max--;
    $("div#max > input").val(t_max);
    layout_all();
}

function create_pie_canvas() {
    pie_elt.append($(document.createElement("canvas")));
    draw_pie();
}

function create_cloud_canvas() {
    cloud_elt.append($(document.createElement("canvas")));
}

function test1a() {
    $.get("http://localhost:8080/tests/test1a.json", load_msg_obj)
}

function test1b() {
    $.get("http://localhost:8080/tests/test1b.json", load_msg_obj)
}

function test1000() {
    $.get("http://localhost:8080/tests/test1000.json", load_msg_obj)
}

function test10000() {
    $.get("http://localhost:8080/tests/test10000.json", load_msg_obj)
}

function _test2(n) {
    function f(o) {
	load_msg_obj(o);
	_test2 (n+1);
    }
    if (n >= 100) { return }
    else $.get("http://localhost:8080/tests/test2-" + n + ".json", f)
}

function test2() {
    _test2(0);
}

function visualise () {
    $.get($("input#json_url").val(), load_msg_obj)
}

create_spinner("min", min_up, min_down);
create_spinner("max", max_up, max_down);

$("button#visualise").click(visualise);
$("button#next_msg").click(next_msg);
$("button#start").click(start);
$("button#test1a").click(test1a);
$("button#test1b").click(test1b);
$("button#test2").click(test2);
$("button#test1000").click(test1000);
$("button#test10000").click(test10000);

create_pie_canvas();
create_cloud_canvas();