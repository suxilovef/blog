;(function () {
  var file = '来自过去的SOS - 无期迷途&沐可linda.mp3'
  function isHome() {
    try {
      if (window.GLOBAL_CONFIG_SITE && GLOBAL_CONFIG_SITE.pageType === 'home') return true
    } catch (e) {}
    var p = location.pathname.replace(/index\.html$/, '')
    if (p === '' || p === '/') return true
    return false
  }
  function buildSrc() {
    var encoded = encodeURIComponent(file)
    return '/musich/' + encoded
  }
  function createAudio() {
    var a = document.createElement('audio')
    a.id = 'home-audio-player'
    a.src = ''
    a.preload = 'none'
    a.loop = false
    a.controls = false
    a.style.display = 'none'
    document.body.appendChild(a)
    return a
  }
  function createBtn() {
    var placement = (window.HOME_AUDIO_PLACEMENT || 'nav').toLowerCase()
    if (placement === 'nav') {
      var item = document.createElement('div')
      item.className = 'menus_item'
      var a = document.createElement('a')
      a.className = 'site-page'
      a.id = 'home-audio-btn'
      a.href = 'javascript:void(0)'
      a.title = '音乐播放'
      a.innerHTML = '<i class="fa-fw fas fa-music"></i><span> 音乐</span>'
      item.appendChild(a)
      return item
    } else {
      var btn = document.createElement('button')
      btn.id = 'home-audio-btn'
      btn.type = 'button'
      btn.title = '音乐播放'
      btn.innerHTML = '<i class="fas fa-music"></i>'
      btn.style.cursor = 'pointer'
      btn.style.marginLeft = '8px'
      return btn
    }
  }
  function mountBtn(btn) {
    var placement = (window.HOME_AUDIO_PLACEMENT || 'nav').toLowerCase()
    if (placement === 'nav') {
      var list = document.querySelector('#nav #menus .menus_items')
      if (list) {
        list.appendChild(btn)
        return
      }
    } else if (placement === 'header') {
      var sub = document.getElementById('subtitle')
      if (sub && sub.parentElement) {
        sub.parentElement.appendChild(btn)
        return
      }
    }
    var host = document.getElementById('rightside-config-show') || document.getElementById('rightside')
    if (host) host.appendChild(btn)
    else {
      btn.style.position = 'fixed'
      btn.style.bottom = '100px'
      btn.style.right = '20px'
      btn.style.zIndex = '9999'
      document.body.appendChild(btn)
    }
  }
  function init() {
    if (!isHome()) return
    if (document.getElementById('home-audio-btn')) return
    var audio = document.getElementById('home-audio-player') || createAudio()
    var btn = createBtn()
    var target = btn.querySelector('#home-audio-btn') || btn
    var playing = false
    target.addEventListener('click', function () {
      if (!playing) {
        audio.src = buildSrc()
        var p = audio.play()
        if (p && typeof p.then === 'function') {
          p.then(function () {
            playing = true
            target.classList.add('playing')
            target.title = '暂停音乐'
          }).catch(function () {})
        } else {
          playing = true
          target.classList.add('playing')
          target.title = '暂停音乐'
        }
      } else {
        audio.pause()
        playing = false
        target.classList.remove('playing')
        target.title = '音乐播放'
      }
    })
    mountBtn(btn)
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init)
  } else {
    init()
  }
  document.addEventListener('pjax:complete', init)
})()
