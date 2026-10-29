// Full-screen spinner shown while Shiny is busy.
// The delay avoids a flash on short or polling-triggered recalculations.
$(function() {
  var overlay = document.createElement('div');
  overlay.id = 'episomer-busy-overlay';
  overlay.innerHTML = '<div class="episomer-spinner"></div>';
  document.body.appendChild(overlay);
  var timer = null;
  $(document).on('shiny:busy', function() {
    clearTimeout(timer);
    timer = setTimeout(function() { overlay.classList.add('active'); }, 400);
  });
  $(document).on('shiny:idle', function() {
    clearTimeout(timer);
    overlay.classList.remove('active');
  });
});
