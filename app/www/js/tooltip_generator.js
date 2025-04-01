$(document).on('DOMContentLoaded', function(event) {

  const body = document.getElementById('body')
  let tooltip = document.getElementById('tooltip')
  const tooltip_content = document.getElementById('tooltip_content')
  let app_title = document.getElementById('app_title')
  let species_or_region = document.getElementById('species_or_region_select')

  app_title.addEventListener('mouseenter', function(event) {
    if(tooltip.style.display == 'none'){
      tooltip.style.display = 'block';
      tooltip_content.innerText = "BC Aquatic Invasive Species Observation Viewer"
    }
  });

  app_title.addEventListener('mouseleave', function() {
    tooltip.style.display = 'none';
  });

  // Track mouse movement, update tooltip(s) location(s).
  window.addEventListener('mousemove', function(e) {
    tooltip.style.top = e.clientY - 70 + 'px';
    tooltip.style.left = e.clientX + 'px';
  })
})
