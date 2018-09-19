import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

console.log(app.ports);

app.ports.jslog.subscribe(str => console.log(str));

window.app = app;

registerServiceWorker();
