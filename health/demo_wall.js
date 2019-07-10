if ((typeof options.color_pal)==="string"){
  options.color_pal = [options.color_pal];
}

if ((typeof options.darken_except) ==="number"){
  options.darken_except = [options.darken_except];
}

d3.select("#tooling").remove();

// get the widths for each bar

d3.rgb.prototype.toHex = function() {
  var r = Math.round(this.r).toString(16);
  var g = Math.round(this.g).toString(16);
  var b = Math.round(this.b).toString(16);
  if(this.r < 16) r = "0" + r;
  if(this.g < 16) g = "0" + g;
  if(this.b < 16) b = "0" + b;
  return "#"+r+g+b;
};

svg.selectAll("*").remove();

var div = d3.select("body").append("div")   
	.attr("class", "tooltip")
	.attr("id","tooling")
	.style("opacity", 0)
	.style("position", "absolute")
	.style("text-align", "left")
	.style("width", "auto")
	.style("height", "auto")
	.style("padding", "5px")
	.style("font", "16px sans-serif")
	.style("background", "#a08ffc")
	.style("border", "2px")
	.style("border-style", "solid")
	.style("border-color", "black")
	.style("border-radius", "8px")
	.style("pointer-events", "none")
	.style("top","0px")
	.style("left","0px");

// get the widths for each gene
var barWidths = [];
for (var i = 0; i < data.length; i++){
	barWidths.push(data[i].Perc_Wid);
}

// get the stacks for the heat map
var stack = d3.stack()
  .keys(options.time_points);

var stacked = stack(data);
	
// y scale (stacking)
var y = d3.scaleLinear()
  .range([height, 0])
  .domain([0, options.heights[options.heights.length-1]]);

 // x scale (widths)
var x = d3.scaleLinear()
  .range([0,width])
  .domain([0, width]);

svg.select('svg.stack');

// build the color array

var color = d3.scaleLinear()
	.domain([-1, 0, 1])
	.range(["blue", "gray", "yellow"]);

//console.log(color(.5));
// bind a <g> tag for each layer
var layers = svg.selectAll('g.layer')
  .data(stacked, function(d) { return d.key; })
	.enter()
	  .append('g')
		.attr('class', 'layer');
		//.attr('fill', function(d) { return color(d.key); });
		
// bind a <rect> to each value inside the layer

var layers_enter = layers.selectAll('rect')
  .data(function(d) { return d; })
  .enter();
  
// placing the bars on the svg
var count = 0;
var count_id = 0;
var counting1 = 0;
var counting2 = 0;
layers_enter.append('rect')
  .attr('id', function(d,i){
    /*
    if (count_id!==0 && count_id % (options.genes.length-1) === 0){
      var store = count_id;
		  count_id = 0;
		  return "heat_box_"+i+"_"+store;
	  } else {
	    count_id += 1;
		  return "heat_box_"+i+"_"+count_id;
	  }
		 */
		 
		 return "heat_box_"+i;
    
  })
	.attr('x', function(d,i) { 
		if (i===0){
			count = 0;
		}
		count += width*barWidths[i];
		return count - (width*barWidths[i]); 
	})
	.attr('width', function(d,i) { return Math.floor(width*barWidths[i]) }) //width / data.length - 1) // fix later
	.attr('y', function(d,i) {
	// remember that SVG is y-down while our graph is y-up!
	// here, we set the top-left of this bar segment to the
	// larger value of the pair
	  var retting = y(options.heights[counting1+1]);
	  
	   // increase count once we've gotten to last gene
	  if (i === options.genes.length-1){
		  counting1 +=1;
	  }
	  
		return retting;//y(d[1]);
	}).attr('height', function(d,i) {
	// since we are drawing our bar from the top downwards,
	// the length of the bar is the distance between our points
	  var ret = y(options.heights[counting2])-y(options.heights[counting2+1]);
	   
	   // increase count once we've gotten to last gene
	  if (i === options.genes.length-1){
		  counting2 +=1;
	  }
	 
		return ret;
	}).attr('fill', function(d,i){
	  var col = d[1]-d[0];
		if (!options.darken || options.darken_except.includes(i)){
		  
		  return(color(col));
	  } else {
	    return d3.rgb(color(col)).darker(4);
	  }
	})
	.attr("stroke", function(d,i) { 
	  var col = d[1]-d[0];
		if (!options.darken || options.darken_except.includes(i)){
		  
		  
		  return(color(col));
	  } else {
	    return d3.rgb(color(col)).darker(4);
	  }
  }).on("mouseover", function(d,i){
		
		div.transition()        
				.duration(0)      
				.style("opacity", 1)
				.style("left", (d3.event.pageX-1) + "px")     
				.style("top", (d3.event.pageY-1) + "px");
				//.style("background",  color(options.fc_cats.indexOf(cat)));
				
		div.html("Hours Shifted:</br>"+options.fc_hours_shifted[i].toFixed(3));
	})
	.on("mouseout", function(d,i) {
    div.transition()        
        .duration(0)      
        .style("opacity", 0);   
    
	});
