function toggle(elt) {
  var elts,j;
  while (elt && !elt.classList.contains("section")) {
    elt = elt.parentElement;
  }
  if (!elt) return;
  elts = elt.querySelectorAll(".section-text, .section-toggle");
  for (j = 0; j < elts.length; j++)
    elts[j].classList.toggle("active");
}

var nodes,i;

nodes = document.getElementsByClassName("section-toggle");
for (i = 0; i < nodes.length; i++) {
  nodes[i].addEventListener("click", function() {
    toggle(this);
  });
}

function focus(h) {
  var elts,e,tk,i;
  elts = document.getElementsByName(h.substring(1));
  for (i=0; i< elts.length; i++) {
    e = elts[i];
    while (e) {
      tk = e.classList;
      if (tk.contains("section-text") && !tk.contains("active")) {
        toggle(e);
        break;
      }
      e = e.parentElement;
    }
  }
}

focus(window.location.hash);
window.addEventListener('hashchange',focus);
