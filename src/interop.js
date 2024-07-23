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
    decisions: [{ left: "🪦", right: "🥺", selection: null}]
  }
  app.ports.sendQuiz.send(quiz)
}