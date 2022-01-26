function wire(app) {
  const {saveInternal, remove, load, loaded, getAllKeys, receivedAllKeys} = app.ports

  saveInternal?.subscribe(([key, value]) => {
      localStorage.setItem(key, JSON.stringify(value))
    }
  )

  remove?.subscribe(key =>
    localStorage.removeItem(key)
  )

  load?.subscribe(key => {
    const item = localStorage.getItem(key)
    loaded.send([key, JSON.parse(item)])
  })

  getAllKeys?.subscribe(() => {
      const keys = [...Array(localStorage.length).keys()]
        .map(n => localStorage.key(n))
      receivedAllKeys.send(keys)
    }
  )
}

const LocalStorageWrapper = {wire}

export default LocalStorageWrapper