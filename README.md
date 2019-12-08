<button class="tablink" onclick="openPage('Home', this, 'red')">Home</button>
<button class="tablink" onclick="openPage('News', this, 'green')" id="defaultOpen">News</button>
<button class="tablink" onclick="openPage('Contact', this, 'blue')">Contact</button>
<button class="tablink" onclick="openPage('About', this, 'orange')">About</button>

<div id="Home" class="tabcontent">
  <h3>Home</h3>
  <p>Home is where the heart is..</p>
  <iframe src="https://ryoungerman.shinyapps.io/ShinyWebApp/" width="100%" height="400px"></iframe>
  
</div>

<div id="News" class="tabcontent">
  <h3>News</h3>
  <p>Some news this fine day!</p>  
  <iframe src="https://ryoungerman.shinyapps.io/AlanaApp/" width="100%" height="400px"></iframe>
</div>

<div id="Contact" class="tabcontent">
  <h3>Contact</h3>
  <p>Get in touch, or swing by for a cup of coffee.</p>
  <iframe src="https://ryoungerman.shinyapps.io/NinonApp/" width="100%" height="400px"></iframe>
</div>

<div id="About" class="tabcontent">
  <h3>About</h3>
  <p>Who we are and what we do.</p>
</div>
