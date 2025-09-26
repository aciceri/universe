var Hyphenopoly = {
	require: {
	    "it": "Supercalifragilistichespiralidoso",
	    "en-us": "Supercalifragilisticexpialidocious"
	},
	setup: {
	    selectors: {
		      "article": {}
	    }
	}
};

document.addEventListener('DOMContentLoaded', (event) => {
    manageDarkMode();
    manageLightBox();
    manageKatex();
    manageAsciinema();
});

function manageDarkMode() {
    let styles = ['theme-light', 'theme-dark', 'theme-yellow'];
    let switcher = document.getElementById('theme-switcher');
    let body = document.body;
    let active;
    if ((active = window.localStorage.getItem('active-theme')) === null)
	      active = 0; //default theme

    function update() {
	      for (let styleName of styles)
	          body.classList.remove(styleName);
	      body.classList.add(styles[active]);
	      //switcher.innerText = styles[(active + styles.length + 1) % styles.length];
    }

    update();

    switcher.onclick = function () {
        active = (active + 1) % styles.length;
	      window.localStorage.setItem('active-theme', active);
        update();
    };
}

function manageLightBox() {
    let scrollPosition = 0;

    modal = document.createElement('div');
    modal.id = 'lightbox';
    modal.onclick = function (event) {
	      event.preventDefault();
	      modal.style.display = 'none';
	      modal.innerHTML = '';

	      document.body.classList.remove('noscroll');
	      document.body.style.top = '';
	      window.scrollTo(0, scrollPosition);
    };
    document.body.appendChild(modal);

    for (let img of document.getElementsByTagName('img')) {
	      img.setAttribute('data-original', 'false');
	      img.title = 'Click to zoom';
	      img.style.cursor = 'pointer';
	      img.onclick = function (event) {
	          event.preventDefault();
	          event.stopPropagation();

	          // Salva la posizione di scroll corrente
	          scrollPosition = window.pageYOffset || document.documentElement.scrollTop;

	          document.body.style.top = -scrollPosition + 'px';
	          document.body.classList.add('noscroll');

	          modal.style.display = 'block';
	          const clonedImg = img.cloneNode();
	          clonedImg.title = 'Click to close';
	          clonedImg.style.cursor = 'pointer';
	          modal.appendChild(clonedImg);
	      };
    }
}

function manageKatex() {
    renderMathInElement(document.body, {
	macros: {
	    "\\R": "\\mathbb{R}",
	    "\\N": "\\mathbb{N}",
	    "\\epsilon": "\\varepsilon",
	    "\\ceil": "\\lceil #1 \\rceil",
	    "\\floor": "\\lfloor #1 \\rfloor"
	}
    });
}

function manageAsciinema() {
    function initAsciinema() {
        console.log('AsciinemaPlayer:', typeof AsciinemaPlayer);
        console.log('window.AsciinemaPlayer:', typeof window.AsciinemaPlayer);

        let player = window.AsciinemaPlayer || AsciinemaPlayer;
        if (player && player.create) {
            console.log('AsciinemaPlayer loaded, initializing players...');

            document.querySelectorAll('.asciinema-container div[id^="cast-"]').forEach(element => {
                let castName = element.id.replace('cast-', '');
                let castPath = '/casts/' + castName + '.cast';
                console.log('Creating player for:', castName, 'at:', castPath);

                try {
                    player.create(castPath, element);
                    console.log('Player created successfully for:', castName);
                } catch (error) {
                    console.error('Error creating player for ' + castName + ':', error);
                }
            });
        } else {
            console.log('AsciinemaPlayer not ready, retrying in 100ms...');
            setTimeout(initAsciinema, 100);
        }
    }

    initAsciinema();
}
