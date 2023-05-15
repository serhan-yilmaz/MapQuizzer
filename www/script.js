function FindPosition(oElement)
{
  if(typeof( oElement.offsetParent ) != "undefined")
  {
    for(var posX = 0, posY = 0; oElement; oElement = oElement.offsetParent)
    {
      posX += oElement.offsetLeft;
      posY += oElement.offsetTop;
    }
      return [ posX, posY ];
    }
    else
    {
      return [ oElement.x, oElement.y ];
    }
}

function RelativePos(obj, event)
{
	var ImgPos;
	ImgPos = FindPosition(obj)
	var x = 0;
	var y = 0;
  if (!e) var e = window.event;
  if (e.pageX || e.pageY)
  {
	x = e.pageX;
	y = e.pageY;
  }
  else if (e.clientX || e.clientY)
    {
      x = e.clientX + document.body.scrollLeft
        + document.documentElement.scrollLeft;
      y = e.clientY + document.body.scrollTop
        + document.documentElement.scrollTop;
    }
	x = x - ImgPos[0];
	y = y - ImgPos[1];
	return [x, y]
}

function getDomain(url, subdomain) {
    subdomain = subdomain || false;

    url = url.replace(/(https?:\/\/)?(www.)?/i, '');

    if (!subdomain) {
        url = url.split('.');

        url = url.slice(url.length - 2).join('.');
    }

    if (url.indexOf('/') !== -1) {
        return url.split('/')[0];
    }

    return url;
}

var dimension = [0, 0, 0, 0];
$(document).on("shiny:connected", function(e) {
	Shiny.setInputValue('initialized', 1);
	dimension[0] = window.innerWidth;
	dimension[1] = window.innerHeight;
	dimension[2] = screen.width;
	dimension[3] = screen.height;	
	Shiny.onInputChange("dimension", dimension);
	
	var el4 = document.getElementById('image');
	var img_dims = [0, 0];
	img_dims[0] = el4.offsetWidth;
	img_dims[1] = el4.offsetHeight;
	Shiny.onInputChange("imgdims", img_dims)
	
	var hostname = [0, 0];
	hostname[0] = getDomain(window.location.href, true);
	hostname[1] = window.location.href;
	Shiny.onInputChange("hostname", hostname)
	console.log(`Window size (${dimension[0]}, ${dimension[1]}) with screen size (${dimension[2]}, ${dimension[3]})`);
	$(window).resize(function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		dimension[2] = screen.width;
		dimension[3] = screen.height;	
		Shiny.onInputChange("dimension", dimension);
		console.log(`Window size (${dimension[0]}, ${dimension[1]}) with screen size (${dimension[2]}, ${dimension[3]})`);
		
		var el = document.getElementById('loadmessage'); 
		console.log(el.offsetHeight)
		// var value = Math.max(el.offsetHeight-1, 50)
		var value = (el.offsetHeight-1)
		// var value = el.offsetHeight
		var el2 = document.getElementById('map_div'); 
		el2.style.marginBottom = value + 'px'
		
		// var el3 = document.getElementById('image');
		// var img_dims = [0, 0];
		// img_dims[0] = el3.offsetWidth;
		// img_dims[1] = el3.offsetHeight;
		// Shiny.onInputChange("imgdims", img_dims)
	});
});


function reset_footer_animation() {
  var el = document.getElementById('loadmessage');
  el.style.animation = 'none';
  el.offsetHeight; /* trigger reflow */
  el.style.animation = null; 
}

function update_image(width, height, clip_class) {
  var el = document.getElementById('imgx');
  el.width = width;
  el.height = height;
	for (let i = el.classList.length - 1; i >= 0; i--) {
		const className = el.classList[i];
		if (className.startsWith('clip-')) {
			el.classList.remove(className);
		}
	}
  if(clip_class){
	  el.classList.add(clip_class);
  }
}

$( document ).ready(function() {
	var el = document.getElementById('loadmessage'); 
	el.addEventListener('onresize', function(e) {
		console.log('abcd')
	});
	var intervalId = setInterval(function() {
		
		if(!document.body.classList.contains('modal-open')){
			document.body.style.overflow = 'auto';
		}
	}, 1000);
});

$( document ).ready(function() {
	var el3 = document.getElementById('image');
	var intervalId2 = setInterval(function() {
		var img_dims = [0, 0];
		img_dims[0] = el3.offsetWidth;
		img_dims[1] = el3.offsetHeight;
		Shiny.onInputChange("imgdims", img_dims)
	}, 500);
});