// This returns the flags passed into your Elm application
export const flags = async ({ env }) => {
  return {}
}

// This function is called once your Elm app is running
export const onReady = ({ app, env }) => {
  app.ports.getQuizFromLocalStorage.subscribe(id => getQuizFromLocalStorage(app, id))
}

export const getQuizFromLocalStorage = (app, id) => {
  let quiz = {
    values: "🪦 🥺 🤠 💪🏻 😰 😛 🕸️ 🦑 🐒 🐷 🐫 🥓 🧋 🥃 🎫".split(" ").map(x => [x]),
    stack: { decisions: [{ left: "🪦", right: "🥺", selection: null}], low: 0, high: 1 },
    lookup: {}
  }
  app.ports.sendQuiz.send(quiz)
}