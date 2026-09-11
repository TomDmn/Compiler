function sectionIcon(folded){
  return folded ? "<i class='fa fa-chevron-down' aria-hidden='true'></i>"
                : "<i class='fa fa-chevron-up' aria-hidden='true'></i>";
}

function toggleSection(id){
  var content = document.getElementById('report-content-' + id);
  var button = document.getElementById('toggle-section-' + id);
  var folded = content.style.display == 'none';
  content.style.display = folded ? 'block' : 'none';
  button.innerHTML = sectionIcon(!folded);
  button.title = folded ? 'Fold section' : 'Unfold section';
  button.setAttribute('aria-label', button.title);
  button.setAttribute('aria-expanded', folded ? 'true' : 'false');
}

function toggleAllSections(){
  var contents = document.querySelectorAll('.report-section-content');
  var fold = false;
  for (var k = 0; k < contents.length; k++){
    if (contents[k].style.display != 'none') {
      fold = true;
      break;
    }
  }
  for (var k = 0; k < contents.length; k++){
    contents[k].style.display = fold ? 'none' : 'block';
  }
  var buttons = document.querySelectorAll('.report-section-toggle');
  for (var k = 0; k < buttons.length; k++){
    buttons[k].innerHTML = sectionIcon(fold);
    buttons[k].title = fold ? 'Unfold section' : 'Fold section';
    buttons[k].setAttribute('aria-label', buttons[k].title);
    buttons[k].setAttribute('aria-expanded', fold ? 'false' : 'true');
  }
  var all = document.getElementById('toggle-all-sections');
  all.innerHTML = sectionIcon(fold);
  all.title = fold ? 'Unfold all sections' : 'Fold all sections';
  all.setAttribute('aria-label', all.title);
}

function setSidebarHidden(hidden){
  document.body.classList.toggle('sidebar-hidden', hidden);
  var button = document.getElementById('toggle-sidebar');
  if (!button) return;
  button.innerHTML = hidden ? "<i class='fa fa-chevron-right' aria-hidden='true'></i>"
                            : "<i class='fa fa-chevron-left' aria-hidden='true'></i>";
  button.title = hidden ? 'Show sidebar' : 'Hide sidebar';
  button.setAttribute('aria-label', button.title);
  button.setAttribute('aria-expanded', hidden ? 'false' : 'true');
}

function toggleSidebar(){
  setSidebarHidden(!document.body.classList.contains('sidebar-hidden'));
}

function setActiveSidebarSection(sectionId){
  var links = document.querySelectorAll('.sidebar-link[data-section-id]');
  for (var index = 0; index < links.length; index++){
    var active = links[index].dataset.sectionId == sectionId;
    var wasActive = links[index].classList.contains('active');
    links[index].classList.toggle('active', active);
    if (active){
      links[index].setAttribute('aria-current', 'location');
      if (!wasActive) links[index].scrollIntoView({block: 'nearest'});
    }
    else links[index].removeAttribute('aria-current');
  }
}

function initializeSidebar(){
  var narrowScreen = window.matchMedia('(max-width: 720px)');
  if (narrowScreen.matches) setSidebarHidden(true);
  if (narrowScreen.addEventListener){
    narrowScreen.addEventListener('change', function(event){
      setSidebarHidden(event.matches);
    });
  }

  var links = Array.prototype.slice.call(
    document.querySelectorAll('.sidebar-link[data-section-id]')
  );
  if (links.length == 0) return;

  links.forEach(function(link){
    link.addEventListener('click', function(){
      setActiveSidebarSection(link.dataset.sectionId);
      if (narrowScreen.matches) setSidebarHidden(true);
    });
  });

  var initialSection = window.location.hash.replace(/^#/, '');
  if (!links.some(function(link){ return link.dataset.sectionId == initialSection; })){
    initialSection = links[0].dataset.sectionId;
  }
  setActiveSidebarSection(initialSection);

  if (!('IntersectionObserver' in window)) return;
  var visibleSections = new Map();
  var observer = new IntersectionObserver(function(entries){
    entries.forEach(function(entry){
      var sectionId = entry.target.dataset.sectionId;
      if (entry.isIntersecting) visibleSections.set(sectionId, entry.target);
      else visibleSections.delete(sectionId);
    });
    if (visibleSections.size == 0) return;
    var targetLine = window.innerHeight * .12;
    var closest = Array.from(visibleSections.entries()).sort(function(left, right){
      return Math.abs(left[1].getBoundingClientRect().top - targetLine)
           - Math.abs(right[1].getBoundingClientRect().top - targetLine);
    })[0];
    setActiveSidebarSection(closest[0]);
  }, {
    rootMargin: '-8% 0px -72% 0px',
    threshold: [0, .1, .5]
  });

  links.forEach(function(link){
    var section = document.getElementById('report-section-' + link.dataset.sectionId);
    if (section) observer.observe(section);
  });
}

function toggleLtlGroups(button){
  var content = document.getElementById('report-content-ltl');
  var hidden = content.classList.toggle('ltl-groups-hidden');
  button.innerHTML = hidden ? "<i class='fa fa-layer-group' aria-hidden='true'></i>"
                            : "<i class='fa fa-bars' aria-hidden='true'></i>";
  button.title = hidden ? 'Show LTL groups' : 'Hide LTL groups';
  button.setAttribute('aria-label', button.title);
  button.setAttribute('aria-pressed', hidden ? 'true' : 'false');
}

function toggleRtlBlocks(button, sectionId){
  var content = document.getElementById('report-content-' + sectionId);
  var hidden = content.classList.toggle('rtl-blocks-hidden');
  button.innerHTML = hidden ? "<i class='fa fa-layer-group' aria-hidden='true'></i>"
                            : "<i class='fa fa-bars' aria-hidden='true'></i>";
  button.title = hidden ? 'Show node blocks' : 'Hide node blocks';
  button.setAttribute('aria-label', button.title);
  button.setAttribute('aria-pressed', hidden ? 'true' : 'false');
}

function toggleRtlLiveness(button, sectionId){
  var content = document.getElementById('report-content-' + sectionId);
  var hidden = content.classList.toggle('rtl-liveness-hidden');
  button.innerHTML = hidden ? "<i class='fa fa-eye' aria-hidden='true'></i>"
                            : "<i class='fa fa-eye-slash' aria-hidden='true'></i>";
  button.title = hidden ? 'Show liveness information' : 'Hide liveness information';
  button.setAttribute('aria-label', button.title);
  button.setAttribute('aria-pressed', hidden ? 'true' : 'false');
}

function loadDeferredImage(button){
  var placeholder = button.closest('.deferred-image');
  if (!placeholder) return;
  var source = placeholder.dataset.imageSrc;
  var link = document.createElement('a');
  link.className = 'report-image-link';
  link.href = source;
  link.target = '_blank';
  link.rel = 'noopener';
  link.title = 'Open diagram';

  var image = document.createElement('img');
  image.className = 'report-image';
  image.src = source;
  image.alt = 'Generated diagram';
  link.appendChild(image);
  placeholder.replaceWith(link);
}

document.addEventListener('DOMContentLoaded', initializeSidebar);
