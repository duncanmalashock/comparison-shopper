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
    options: "3️⃣ 2️⃣ 4️⃣ 5️⃣ 1️⃣".split(" "),
    preferences: {}
  }
  app.ports.sendQuiz.send(quiz)
}