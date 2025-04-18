function delete_old_minibuttons(){
  old_minibuttons = document.getElementsByClassName('shiny-minibutton')
  if(old_minibuttons != null){
    // Various buttons with different IDs
    if(old_minibuttons.length > 1){
      console.log('found 2+ different old minibutton. Removing...');
      for(let i = 0; i < old_minibuttons.length; i++){
        old_minibuttons[i].remove()
        }
     }
  }
}

const observer = new MutationObserver(function(mutations, observer) {
  // Find leaflet elements
  let doc_body = document.getElementsByTagName('body')[0];
  let leaf_window = document.getElementById('leafmap');
  let leaf_legends = document.getElementsByClassName('legend');

  if(leaf_legends[1].style.maxHeight != leaf_window.offsetHeight * 0.6 + 'px') {

  console.log("Number of leaf legends: " + leaf_legends.length)

  wdims = leaf_window.getBoundingClientRect()

  delete_old_minibuttons();

  for (let i = 0; i < leaf_legends.length; i++) {

    let ll = leaf_legends[i]
    let legend_p_dims = ll.parentElement.getBoundingClientRect()
    let ldims = ll.getBoundingClientRect()

    console.log('setting styles for leaflet legend ' + i);
    //leaf_legends[i].style.overflowY = 'scroll';
    ll.style.overflowY = 'scroll';
    ll.style.overflowX = 'hidden';
    ll.style.transition = 'all 0.5s linear';
    ll.style.border = 'solid 1px black';
    ll.style.maxHeight = leaf_window.offsetHeight * 0.6 + 'px';

    // Check to see if there's an old minibutton from a previous leaflet map instance.
    // If so, delete it.
    let defunct_button = document.getElementById('minibutton-' + i);
    if(defunct_button != null){
      defunct_button.remove()
    }

    // Create minibutton instance and assign static styling parameters.
    let minibutton = document.createElement('div');
    let mb_bg = document.createElement('img');
    minibutton.id = 'minibutton-' + i;
    minibutton.style.zIndex = 1000;
    minibutton.style.top = ll.getBoundingClientRect().y + ll.offsetHeight + 'px';
    minibutton.style.left = ll.getBoundingClientRect().x + 'px';
    minibutton.classList.add('shiny-minibutton');
    mb_bg.classList.add('minibutton-background');
    minibutton.appendChild(mb_bg);
    doc_body.appendChild(minibutton);

    // Add observer for minibutton.
    document.getElementById('minibutton-' + i).addEventListener('click', function() {
      leaf_legends[i].classList.toggle('shrink-leg');
      document.getElementById('minibutton-' + i).classList.toggle('minibutton-closed');
      if(document.getElementById('minibutton-' + i).classList.contains('minibutton-closed')){
        document.getElementById('minibutton-' + i).style.transform = 'translateY(-' + ll.offsetHeight/2 + 'px)'
        document.getElementById('minibutton-' + i).children[0].style.transform = 'rotate(0deg)'
      } else {
        document.getElementById('minibutton-' + i).style.transform = '';
        document.getElementById('minibutton-' + i).children[0].style.transform = 'rotate(180deg)';
      }
    })

    console.log('styles set and child appended');
  }
    }
  //observer.disconnect(); // Stop observing once legends are styled
});

// Observe changes in the entire document (can be scoped to a specific div)
observer.observe(document.body, { childList: true, subtree: true });
