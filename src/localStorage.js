function wire(app) {
  const storage = (window).localStorage
  const {saveInternal, remove, load, loaded, getAllKeys, receivedAllKeys} = app.ports

  saveInternal?.subscribe(([key, value]) => {
      console.log("saveInternal: key, value: ", key, value)
      localStorage.setItem(key, JSON.stringify(value))
    }
  )

  remove?.subscribe(key =>
    localStorage.removeItem(key)
  )

  load?.subscribe(key => {
    const item = localStorage.getItem(key)
    console.log("load: key, item", key, item, JSON.parse(item))
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