var visualiser_elt = $("div#visualiser");
var messages = {};
var functions = {};
var threads = {};

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
	layout = true;
    }

    if(t > t_max) {
	t_max = t;
	layout = true;
    }

    if(layout) {
	layout_all();
    }
}

function load_msg_obj(o) {
    var msgs = eval(o);
    for(m in msgs) {
	var msg = msgs[m];
	set_t(msg.ts);

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

$("button#next_msg").click(next_msg);
$("button#start").click(start);