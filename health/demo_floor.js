// MAKE CHORD DIAGRAM


var split_perc = 0.5;

d3.rgb.prototype.toHex = function() {
  var r = Math.round(this.r).toString(16);
  var g = Math.round(this.g).toString(16);
  var b = Math.round(this.b).toString(16);
  if(this.r < 16) r = "0" + r;
  if(this.g < 16) g = "0" + g;
  if(this.b < 16) b = "0" + b;
  return "#"+r+g+b;
};

d3.select("#tooling").remove();
d3.select("#tool_chord").remove();
d3.select("#tool_arc").remove();
d3.select("#tool_ont").remove();

svg.attr("id","mysvg");

var wid_offset = $(window).width() - width;
var height_offset = 120; // through testing // $(document).height() - height;

svg.selectAll("*").remove();

if ((typeof options.color_pal)==="string"){
  options.color_pal = [options.color_pal];
}

if ((typeof options.color_pal_dark)==="string"){
  options.color_pal_dark = [options.color_pal_dark];
}

if ((typeof options.color_pal_chord)==="string"){
  options.color_pal_chord = [options.color_pal_chord];
}

if ((typeof options.color_pal_dark_chord)==="string"){
  options.color_pal_dark_chord = [options.color_pal_dark_chord];
}

if ((typeof options.path_trace)==="string"){
  options.path_trace = [options.path_trace];
}

if ((typeof options.fc_cats)==="string"){
  options.fc_cats = [options.fc_cats];
}

if ((typeof options.fc_cats_go)==="string"){
  options.fc_cats_go = [options.fc_cats_go];
}

if ((typeof options.fc_full_names)==="string"){
  options.fc_full_names = [options.fc_full_names];
}

if ((typeof options.fc_names)==="string"){
  options.fc_names = [options.fc_names];
}

if ((typeof options.from_fc_cat)==="string"){
  options.from_fc_cat = [options.from_fc_cat];
}

if ((typeof options.from_genes)==="string"){
  options.from_genes = [options.from_genes];
}

if ((typeof options.tot_fc_cats) === "number"){
  options.tot_fc_cats = [options.tot_fc_cats];
}

if ((typeof options.fc_rep) === "string"){
  options.fc_rep = [options.fc_rep];
}


svg.append("rect")
    .attr("width", width)
    .attr("height", height)
    .attr("fill", theme.background);

// make the matrix for the chord diagram
if (typeof(options.genes)=="string"){
  options.genes = [options.genes]
}

var mtx = [];
for (var i = 0; i < options.genes.length; i++){
  mtx.push([]);
  for (var j = 0; j < options.genes.length; j++){
    mtx[i].push(options.chord_dat[options.genes[i]][j]);
  }
}

var matrix = mtx;

d3.rgb.prototype.toHex = function() {
  var r = Math.round(this.r).toString(16);
  var g = Math.round(this.g).toString(16);
  var b = Math.round(this.b).toString(16);
  if(this.r < 16) r = "0" + r;
  if(this.g < 16) g = "0" + g;
  if(this.b < 16) b = "0" + b;
  return "#"+r+g+b;
};

// coloring and such for the tooltip on each chord
var div_chord = d3.select("body").append("div")   
	.attr("class", "tooltip") 
	.attr("id","tool_chord")              
	.style("opacity", 0)
	.style("position", "absolute")
	.style("text-align", "left")
	.style("width", "auto")
	.style("height", "auto")
	.style("padding", "5px")
	.style("font", options.font_size +"px sans-serif")
	.style("background", "pink")
	.style("border", "2px")
	.style("border-style", "solid")
	.style("border-color", "black")
	.style("border-radius", "8px")
	.style("pointer-events", "none")
	.style("top","0px")
	.style("left","0px");

// coloring and such for each tooltip on an arc    
var div_arc = d3.select("body").append("div")   
	.attr("class", "tooltip") 
	.attr("id","tool_arc")              
	.style("opacity", 0)
	.style("position", "absolute")
	.style("text-align", "left")
	.style("width", "auto")
	.style("height", "auto")
	.style("padding", "5px")
	.style("font", options.font_size +"px sans-serif")
	.style("background", "steelblue")
	.style("border", "2px")
	.style("border-style", "solid")
	.style("border-color", "black")
	.style("border-radius", "8px")
	.style("pointer-events", "none")
	.style("top","0px")
	.style("left","0px");    

// setting the chord width
var outerRadius = (Math.min(width, height) *split_perc)+5-10,
	innerRadius = outerRadius - 30;

var formatValue = d3.formatPrefix(",.0", 1e3);

var chord = d3.chord()
	//.padAngle([0.01])
	.sortSubgroups(d3.descending);

var arc = d3.arc()
	.innerRadius(innerRadius)
	//.padAngle(0.01)
	.startAngle(start_angle)
	.endAngle(end_angle)
	.outerRadius(outerRadius);

var ribbon = d3.ribbon()
	.startAngle(start_angle)
	.endAngle(end_angle)
	.radius(innerRadius);

var color_array = [];
var addfactor = 1/(options.fc_cats.length+2);
//color_array.push(d3.interpolateBuPu(1/(options.fc_cats.length)));
for (var i = 1; i <= options.fc_cats.length; i++){
	color_array.push(d3.interpolateBuPu(addfactor*i));
}

var color = d3.scaleOrdinal()
  .range(options.color_pal_chord);
	//s.range(color_array.slice(0,options.fc_cats.length).reverse());

var color_dark = d3.scaleOrdinal()
  .range(options.color_pal_dark_chord);

var gstep = svg.append("g")
  .attr("transform", "translate(" + width / 2 + "," + height / 2 + ") scale(-1,-1)")
  .datum(chord(matrix));

var group = gstep.append("g")
	.attr("class", "groups")
  .selectAll("g")
  .data(function(chords) { return chords.groups; })
  .enter().append("g");

// provide which arcs go to which indices
var arc_inds = [];
for (var i = 0; i < options.fc_cats.length; i++){
  arc_inds.push([]);
}

if (typeof(options.from_genes) == "string"){
  options.from_genes = [options.from_genes]
}

if (typeof(options.from_fc_cat) == "string"){
  options.from_fc_cat = [options.from_fc_cat]
}

// outside arcs
var set_time_const; // constant for hovering to darken heatmap
group.append("path")
	.style("fill", function(d,i) { // also adds gene indices for hover 
	  var genen = options.genes[d.index];
	  var ind = options.from_genes.indexOf(genen);
	  var cat = options.from_fc_cat[ind];
	  
	  arc_inds[options.fc_cats.indexOf(cat)].push(i);
	  return color(options.fc_rep.indexOf(cat)); 
	})
	.style("stroke", function(d) { 
	  var genen = options.genes[d.index];
	  var ind = options.from_genes.indexOf(genen);
	  var cat = options.from_fc_cat[ind];
	  return d3.rgb(color(options.fc_rep.indexOf(cat))).darker().darker(); })
	.attr("d", arc)
	.on("mouseover", function(d,i){
	  clearTimeout(set_time_const);
	  var genen = options.genes[d.index];
	  var ind = options.from_genes.indexOf(genen);
	  var cat = options.from_fc_cat[ind];
	  
	  div_arc.transition()        
			.duration(0)      
			.style("opacity", 0.9)
			.style("left", (d3.event.pageX-10) + "px")     
			.style("top", (d3.event.pageY-10) + "px")
			.style("background",  color(options.fc_rep.indexOf(cat)));
			
		var htmlstr = options.genes[d.index]+" ("+keep[i].length+")";
		
		div_arc.html(htmlstr);
		
	  fade(0,i);
	  fadeHeatArc(0,i);
	  
	  set_time_const = setTimeout(function() {
		    Shiny.setInputValue(
				"dark", // input name
				i, // input value
				{priority: "event"}
			  );
		  }, 1000);
  })
  .on("mouseout", function(d,i) {
    clearTimeout(set_time_const);
    
    div_arc.transition()        
        .duration(0)      
        .style("opacity", 0);
    fade(1,i);
    fadeHeatArc(1,i);
     console.log("hey");
    set_time_const = setTimeout(function(){
        Shiny.setInputValue(
  				"undark", // input name
  				i, // input value
  				{priority: "event"}
  			  );
        }, 1000);
  })
  .on("click",(function(d){
		  Shiny.setInputValue(
			"url", // input name
			options.genes[d.index], // input value
			{priority: "event"}
		  )})
		);
  
  
  // FUNCTIONS FOR FADING CHORDS
  function fade(opacity,our_ind){
    svg.selectAll(whichToFade(our_ind))
          .transition()
          .duration(1000)
          .style("opacity", opacity);
  }
  
  function whichToFade(our_ind){
    var fade_these = [];
    
    for (var i = 0; i < options.tot_chords; i++){
      if (!keep[our_ind].includes(i)){
        fade_these.push("#rib_chords"+i);
      }
    }
    
    return(fade_these);
  }
  
  function fadeGroup(opacity,mult_ind){
    svg.selectAll(whichToFadeGroup(mult_ind))
          .transition()
          .duration(1000)
          .style("opacity", opacity);
  }
  
  function whichToFadeGroup(mult_ind){
    var fade_these = [];
    
    var all_keep = [].concat.apply([],mult_ind.map(function(i){return keep[i]}))
    
    for (var i = 0; i < options.tot_chords; i++){
      if (!all_keep.includes(i)){
        fade_these.push("#rib_chords"+i);
      }
    }
    
    return(fade_these);
  }
  
  function findOurInd(rib_i, is_source){
    var skip = is_source;
    for (var i = 0; i < keep.length; i++){
      if (keep[i].includes(rib_i)){
        if (skip){
          return i;
        } else {
          skip = true;
        }
      }
    }
    
    return 0;
  }
  
  // FUNCTIONS FOR FADING HEATMAPS
  function fadeHeatArc(opacity,our_ind){
    svg.selectAll(whichToFadeHeatArc(our_ind))
      .transition()
      .duration(1000)
      .style("opacity", opacity);
  }
  
  function whichToFadeHeatArc(our_ind){
    var fade_these = [];
    
    for (var i = 0; i < options.from_genes.length; i++){
      if (i!==our_ind){
        fade_these.push("#heatmap"+i);
      }
    }
    
    return(fade_these);
  }
  
  function fadeHeatArcGroup(opacity,mult_ind){
    svg.selectAll(whichToFadeHeatArcGroup(mult_ind))
      .transition()
      .duration(1000)
      .style("opacity", opacity);
  }
  
  function whichToFadeHeatArcGroup(mult_ind){
    var fade_these = [];
    
    for (var i = 0; i < options.from_genes.length; i++){
      if (!mult_ind.includes(i)){
        fade_these.push("#heatmap"+i);
      }
    }
    
    return(fade_these);
  }
  
var keep = [];
var k_counts = [];
// chord connections
gstep.append("g")
.attr("class", "ribbons")
.selectAll("path")
.data(function(chords) { return chords; })
.enter().append("path")
  .attr("id",function (d,i) {
    if (keep[d.source.index] === undefined){
      keep[d.source.index] = [i];
      k_counts[d.source.index] = 1;
    } else {
      keep[d.source.index].push(i);
      k_counts[d.source.index]++;
    }
    
    if (keep[d.target.index] === undefined){
      keep[d.target.index] = [i];
      k_counts[d.target.index] = 1;
    } else {
      keep[d.target.index].push(i);
      k_counts[d.target.index]++;
    }
    
    return "rib_chords"+i;})
  .attr("startAngle",function(d){return d.startAngle -0.005})
  .attr("d", ribbon)
  .attr("fill", function(d) { 
    //source
    var genen = options.genes[d.source.index];
    var ind = options.from_genes.indexOf(genen);
    var cat = options.from_fc_cat[ind];
    
    // target
	  var genen_t = options.genes[d.target.index];
	  var ind_t = options.from_genes.indexOf(genen_t);
	  var cat_t = options.from_fc_cat[ind_t];
	  
	  // if the color is connected to itself, we make it darker
	  if (cat === cat_t){
	    // STOP HERE
	    //console.log(options.fc_rep + " " + options.fc_cats + " " + cat + " " + options.fc_cats.indexOf(cat) + " " + options.fc_rep.indexOf(cat) + " " + color_dark(options.fc_cats.indexOf(cat)) + " " + color_dark(options.fc_rep.indexOf(cat)))
	    return color_dark(options.fc_rep.indexOf(cat)); // d3.rgb(color(options.fc_cats.indexOf(cat))).darker();
	  } else {
	    return color(options.fc_rep.indexOf(cat)); 
	  }
  })
  .attr("stroke", function(d) { 
    var genen = options.genes[d.source.index];
    var ind = options.from_genes.indexOf(genen);
    var cat = options.from_fc_cat[ind];
    
    return d3.rgb(color(options.fc_rep.indexOf(cat))).darker().darker(); })
  .attr("fill-opacity", 0.9)
  .on("mouseover", function(d,i){
	  clearTimeout(set_time_const);
    // WNDOW RESIZING IS THE PROBLEM
    var wid_offset = $(window).width() - width;
    
    var which_col = which_gene(d3.event.pageX - wid_offset, d3.event.pageY - height_offset, 
      d.source.startAngle, d.source.endAngle, d.target.startAngle, d.target.endAngle,
      innerRadius, height, width)
    
    if (which_col === "target"){
      var genen = options.genes[d.target.index];
			var htmlstr = options.genes[d.target.index]+"<br/>";
			
    } else {
      var genen = options.genes[d.source.index];
			var htmlstr = options.genes[d.source.index]+"<br/>";
        
    }
    
    var ind = options.from_genes.indexOf(genen);
    var cat = options.from_fc_cat[ind];
    
    div_chord.transition()        
			.duration(0)      
			.style("opacity", 0.9)
			.style("left", (d3.event.pageX-10) + "px")     
			.style("top", (d3.event.pageY-10) + "px")
			.style("background",  color(options.fc_rep.indexOf(cat)));
			
		div_chord.html(htmlstr);
		
    fade(0,findOurInd(i, which_col === "target"));
    fadeHeatArcGroup(0,[d.source.index,d.target.index]);
    
    set_time_const = setTimeout(function() {
        Shiny.setInputValue(
  				"dark", // input name
  				[d.source.index,d.target.index], // input value
  				{priority: "event"}
			  );
      }, 1000);
  })
  .on("mouseout", function(d,i) {
	  clearTimeout(set_time_const);
    
    var wid_offset = $(window).width() - width;
    var which_col = which_gene(d3.event.pageX - wid_offset, d3.event.pageY - height_offset, 
      d.source.startAngle, d.source.endAngle, d.target.startAngle, d.target.endAngle,
      innerRadius, height, width)
      
    div_chord.transition()        
        .duration(0)      
        .style("opacity", 0);   
    // STOP HERE
    fade(1,findOurInd(i, which_col === "target"));
    fadeHeatArcGroup(1,[d.source.index,d.target.index]);
    
    set_time_const = setTimeout(function() {
        Shiny.setInputValue(
  				"undark", // input name
  				[d.source.index,d.target.index], // input value
  				{priority: "event"}
			  );
      }, 1000);
  })
  .on("click",(function(d,i){
    var wid_offset = $(window).width() - width;
    var which_col = which_gene(d3.event.pageX - wid_offset, d3.event.pageY - height_offset,
		d.source.startAngle, d.source.endAngle, d.target.startAngle, d.target.endAngle,
		innerRadius, height, width)

		if (which_col === "source"){
		  var genen = options.genes[d.target.index];
		  
		} else {
		  var genen = options.genes[d.source.index];
		  
		}
		
	  Shiny.setInputValue(
			"url", // input name
			genen, // input value
			{priority: "event"}
		  )
  })
	);
	
// big outside arc
var bigarc = d3.arc()
	.innerRadius(outerRadius)
	.outerRadius(outerRadius + 10);

var groups = [];
var start = 0;
if ((typeof options.fc_cats) !== "string"){
	for (var i = 0; i < options.fc_cats.length; i++){
	  groups.push({sind: start, eind: start+options.tot_fc_cats[i]-1, title: options.fc_cats[i], color: color(i)});
	  start+=options.tot_fc_cats[i];
	}
} else {
  var i = 0;
  groups.push({sind: start, eind: start+options.tot_fc_cats-1, title: options.fc_cats, color: color(i)});
	  start+=options.tot_fc_cats;
}
//var groups = [
//  {sind: 0, eind: data.length-1, color: color(1)}
//];


var cgrp = chord(matrix).groups;

//draw arcs
for(var i = 0; i< groups.length; i++) {
	var __g = groups[i];
	
	if (options.tot_fc_cats[i]!==undefined && options.tot_fc_cats[i] > 0){
		var arc1 = d3.arc()
			.innerRadius(outerRadius)
			.outerRadius(outerRadius + 10)
			.startAngle(cgrp[__g.sind].startAngle) 
			.endAngle(cgrp[__g.eind].endAngle);
	  
		gstep.append("path")
		  .attr("d", arc1)
		  .attr('fill', __g.color)
		  .attr('id', 'groupId' + i)
		  .on("mouseover", function(d,ii){
	      clearTimeout(set_time_const);
  		  fadeGroup(0,arc_inds[Number(this.id[this.id.length-1])]);
  		  fadeHeatArcGroup(0,arc_inds[Number(this.id[this.id.length-1])]);
    
        set_time_const = setTimeout(function() {
    		    Shiny.setInputValue(
      				"dark", // input name
      				heat_group, // input value
      				{priority: "event"}
    			  )
    		  }, 1000);
      })
      .on("mouseout", function(d,ii) {
	      clearTimeout(set_time_const);
        fadeGroup(1,arc_inds[Number(this.id[this.id.length-1])]);
        fadeHeatArcGroup(1,arc_inds[Number(this.id[this.id.length-1])]);
    
        set_time_const = setTimeout(function() {
    		    Shiny.setInputValue(
      				"undark", // input name
      				heat_group, // input value
      				{priority: "event"}
    			  )
    		  }, 1000);
      });
      
		// Add a text label.
		
		//var text = g.append("text")
			//.attr("dy", 20)
	  //console.log((cgrp[__g.sind].startAngle + cgrp[__g.sind].endAngle)/2*180/Math.PI);
		//text.append("textPath")
		//  .attr("transform", function(d){
		//  var x1 = arc1.centroid(arc1)[0];
		//		var y1 = arc1.centroid(arc1)[1];
		//		//console.log(cgrp[__g.sind].endAngle*180/Math.PI)
		//  return "translate(" +  x1 + "," + y1 + ")rotate(" + computeTextRotationBasic(cgrp[__g.sind].startAngle,cgrp[__g.eind].endAngle) + ")"; 
		//  })
		//	.attr("stroke","#000")
		//	.attr('fill', '#000')
		//	.attr("dy", ".4em")
		//	.style('text-anchor', "middle")
		//	.style('font-family', 'sans-serif')
		//	.attr("xlink:href","#groupId" + i)
		//	.text(__g.title);
	}
		
}



// pretty colors!
//var color_array = ["#98abc5",  "#7b6888", "#6b486b", "#d0743c", "#ff8c00"]
 // building the color interpolation array
var color_array = [];
var addfactor = 1/(options.fc_cats.length+2);
//color_array.push(d3.interpolateBuPu(1/(options.fc_cats.length)));

for (var i = 1; i <= options.fc_cats.length; i++){
	color_array.push(d3.interpolateBuPu(addfactor*i))
}

var coloring = d3.scaleOrdinal()
  .range(options.color_pal);

// legend_fc for fc categories
/*
var last_text;
var legend_fc = svg.append("g")
	.selectAll("g")
	.data(options.fc_cats)
	.enter().append("g")
	  .attr("transform", function(d, i) { 
		last_text = i
		return "translate("+(-width/2+60) +"," + (20 * (i) - Math.floor(height/2)+2) + ")"; });
	  
// add rectangle to legend_fc
legend_fc.append("g")
  .attr('class','triangles')
  .append('path')
  .attr('d', function(d) {return "M 0 0 L 0 18 L 18 0 Z";})
  .attr("fill", function(d,i) {
	  if (d !== ""){ 
	  return options.color_pal[i]; } 
	  else {return "#000000"}
	});
	
legend_fc.append("g")
  .attr('class','triangles')
  .append('path')
  .attr('d', function(d) {return "M 0 18 L 18 18 L 18 0 L 0 18";})
  .attr("fill", function(d,i) {
	  if (d !== ""){ 
	    
	  return options.color_pal_dark[i]; } 
	  else {return "#000000"}
	});


	
// add text next to rectangle
legend_fc.append("text")
  .attr("x", 24)
  .attr("y", 9)
	.attr("fill", theme.foreground)
  .attr("dy", "0.35em")
  .attr("font-family", "sans-serif")
  .text(function(d) { if (d !== ""){return d} });

//var go_name = [options.go_name];


if (options.togg_path){ // toggle based on whether path is wanted or not
  var max_name = Math.max.apply(null,options.path_trace.map(function (x) {return x.length}));
  var legend_name = g.selectAll(null)
  	.data(options.path_trace)
  	.enter().append("text")
  	  .attr("text-anchor", "end")
  		.attr("font-family", "sans-serif")
  		.attr("font-size", "16px")
  		.attr("font-weight", function(d,i){
  		  if (i === 0){
  		    return "bold";
  		  }
  		})
  		.attr("x", width/2-(max_name))
  		.attr("y", function(d,i) {return -height/2+20+(20*i)})
  		.attr("fill", theme.foreground)
  		.text(function(d) {return d})
}
	
var instructions = [];
  //["Each chord indicates a protein-protein interaction",
  //"between two genes. Darker chord colors indicate",
  //"connections within category. More information on",
  //"hover/click interactions appears in Instructions tab."];
var legend_instr = g.selectAll(null)
	.data(instructions)
	.enter().append("text")
	  .attr("font-family", "sans-serif")
		.attr("x", -width/2+2)
		.attr("y", function(d,i) {return height/2-64+(20*i)})
		.attr("fill", theme.foreground)
		.text(function(d) {return d});
	
*/

// FUNCTIONALITY
 
// function that computes which gene card to display on chord (display the closer one)
function which_gene(xc, yc, sAfrom, eAfrom, sAend, eAend, innerRad, height, width){
  var xfrom = width/2 + (innerRad*Math.cos(conv_ang((sAfrom + eAfrom) / 2)))
  var yfrom = (height/2 + (innerRad*Math.sin(conv_ang((sAfrom + eAfrom) / 2))))

  var xend = width/2 + (innerRad*Math.cos(conv_ang((sAend + eAend) / 2)))
  var yend = (height/2 + (innerRad*Math.sin(conv_ang((sAend + eAend) / 2))))
  
  var heightpad = (height/2) - innerRad
  var widthpad = (width/2) - innerRad
  
  var dist_from = euc_dist(xc,height-(yc),xfrom,yfrom)
  var dist_end = euc_dist(xc,height-(yc),xend,yend)
  
  // if it's closer to the from, display the opposite
  if (dist_from < dist_end){
	return "target"
  } else {
	return "source"
  }
}

function conv_ang(ang){
  return (-ang + (Math.PI/2));
}

// function to compute euclidean distance
function euc_dist(xc, yc, xend, yend){
  return Math.sqrt(Math.pow((xc-xend),2)+ Math.pow((yc-yend),2))
}
 
 
function start_angle(d){
  return d.startAngle + .003;
}

function end_angle(d){
  return d.endAngle - .003;
}

function computeTextRotation(d) {
	var angle = (d.startAngle + d.endAngle) / Math.PI * 90;  // <-- 1
	// Avoid upside-down labels
	return (angle < 90 || angle > 270) ? angle : angle + 180;  // <--2 "labels aligned with slices"

	// Alternate label formatting
	//return (angle < 180) ? angle - 90 : angle + 90;  // <-- 3 "labels as spokes"
	//return 0
}	

function computeTextRotationBasic(startAngle,endAngle) {
	var angle = (startAngle + endAngle) / Math.PI * 90;  // <-- 1
	// Avoid upside-down labels
	return (angle < 90 || angle > 270) ? angle : angle + 180;  // <--2 "labels aligned with slices"

	// Alternate label formatting
	//return (angle < 180) ? angle - 90 : angle + 90;  // <-- 3 "labels as spokes"
	//return 0
}	

function square(x) {
  return x * x;
}


function radial() {
  var linear = d3.scaleLinear();

  function scale(x) {
	return Math.sqrt(linear(x));
  }

  scale.domain = function(_) {
	return arguments.length ? (linear.domain(_), scale) : linear.domain();
  };

  scale.nice = function(count) {
	return (linear.nice(count), scale);
  };

  scale.range = function(_) {
	return arguments.length ? (linear.range(_.map(square)), scale) : linear.range().map(Math.sqrt);
  };

  scale.ticks = linear.ticks;
  scale.tickFormat = linear.tickFormat;

 return scale;
}

function getSum(total, num) {
	return total + num;
}

