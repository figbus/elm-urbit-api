import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: Math.floor((Math.random() - 0.5) * 4294967295),
});

app.ports.setupUrbitEventSource.subscribe((url) => {
  const eventSource = new EventSource(url, { withCredentials: true });

  eventSource.onmessage = function (event) {
    app.ports.onUrbitMessage.send({ message: event });
  };

  eventSource.onerror = function (event) {
    app.ports.onUrbitMessage.send({ error: event });
  };
});
