;(function () {
  function isHome() {
    try {
      if (window.GLOBAL_CONFIG_SITE && GLOBAL_CONFIG_SITE.pageType === 'home') return true
    } catch (e) {}
    var p = location.pathname.replace(/index\.html$/, '')
    if (p === '' || p === '/') return true
    if (document.getElementById('recent-posts')) return true
    return false
  }
  function isMobile() {
    if (window.RAIN_ENABLE_MOBILE === true) return false
    return /Mobi|Android|iPhone|iPad|iPod/i.test(navigator.userAgent)
  }
  function createCanvas() {
    var c = document.createElement('canvas')
    c.id = 'raindrops-canvas'
    c.style.position = 'fixed'
    c.style.top = '0'
    c.style.left = '0'
    c.style.width = '100%'
    c.style.height = '100%'
    c.style.pointerEvents = 'none'
    c.style.zIndex = '9999'
    document.body.appendChild(c)
    return c
  }
  function Raindrops(canvas) {
    this.canvas = canvas
    this.ctx = canvas.getContext('2d')
    this.dpr = Math.max(window.devicePixelRatio || 1, 1)
    this.drops = []
    this.splashes = []
    this.running = false
    this.gravity = 0.15
    this.windBase = 0.6
    this.windVar = 0.4
    this.time = 0
    this.resize()
    this.initDrops()
    this.loop = this.loop.bind(this)
    window.addEventListener('resize', this.resize.bind(this))
    var self = this
    document.addEventListener('visibilitychange', function () {
      if (document.hidden) self.stop()
      else self.start()
    })
  }
  Raindrops.prototype.resize = function () {
    var w = window.innerWidth
    var h = window.innerHeight
    this.canvas.width = Math.floor(w * this.dpr)
    this.canvas.height = Math.floor(h * this.dpr)
    this.ctx.setTransform(1, 0, 0, 1, 0, 0)
    this.ctx.scale(this.dpr, this.dpr)
  }
  Raindrops.prototype.initDrops = function () {
    var count = Math.max(Math.floor(window.innerWidth / 9), 60)
    var i = 0
    this.drops.length = 0
    for (i = 0; i < count; i++) {
      this.drops.push(this.spawn())
    }
  }
  Raindrops.prototype.spawn = function () {
    var w = window.innerWidth
    var h = window.innerHeight
    var vy = 1.5 + Math.random() * 3.5
    var len = 10 + Math.random() * 22
    var x = Math.random() * w
    var y = -Math.random() * h
    var opacity = 0.35 + Math.random() * 0.25
    var width = 0.8 + Math.random() * 1.6
    var vx = this.windBase + (Math.random() - 0.5) * this.windVar
    return { x: x, y: y, vx: vx, vy: vy, len: len, opacity: opacity, width: width }
  }
  Raindrops.prototype.update = function () {
    this.time += 1
    var w = window.innerWidth
    var h = window.innerHeight
    for (var i = 0; i < this.drops.length; i++) {
      var d = this.drops[i]
      var wind = this.windBase + Math.sin(this.time * 0.002) * this.windVar
      d.vy += this.gravity
      d.vx = wind
      d.x += d.vx
      d.y += d.vy
      if (d.x > w + 20) d.x = -20
      else if (d.x < -20) d.x = w + 20
      if (d.y - d.len > h) {
        this.splashes.push({ x: d.x, y: h - 2, r: 0, alpha: 0.5 + Math.random() * 0.2, growth: 0.9 + Math.random() * 0.6 })
        this.drops[i] = this.spawn()
      }
    }
    for (var j = this.splashes.length - 1; j >= 0; j--) {
      var s = this.splashes[j]
      s.r += s.growth
      s.alpha *= 0.92
      if (s.alpha < 0.02 || s.r > 18) this.splashes.splice(j, 1)
    }
  }
  Raindrops.prototype.draw = function () {
    var ctx = this.ctx
    var w = window.innerWidth
    var h = window.innerHeight
    ctx.clearRect(0, 0, w, h)
    ctx.save()
    ctx.globalCompositeOperation = 'lighter'
    ctx.lineCap = 'round'
    for (var i = 0; i < this.drops.length; i++) {
      var d = this.drops[i]
      var k = d.len / Math.max(d.vy, 0.001)
      var ex = d.x - d.vx * k
      var ey = d.y - d.vy * k
      var grad = ctx.createLinearGradient(d.x, d.y, ex, ey)
      grad.addColorStop(0, 'rgba(190, 200, 255,' + d.opacity + ')')
      grad.addColorStop(1, 'rgba(190, 200, 255, 0)')
      ctx.strokeStyle = grad
      ctx.lineWidth = d.width
      ctx.beginPath()
      ctx.moveTo(d.x, d.y)
      ctx.lineTo(ex, ey)
      ctx.stroke()
    }
    ctx.restore()
    ctx.save()
    for (var j = 0; j < this.splashes.length; j++) {
      var s = this.splashes[j]
      ctx.strokeStyle = 'rgba(180, 180, 200,' + s.alpha + ')'
      ctx.lineWidth = 1
      ctx.beginPath()
      ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2)
      ctx.stroke()
    }
    ctx.restore()
  }
  Raindrops.prototype.loop = function () {
    if (!this.running) return
    this.update()
    this.draw()
    requestAnimationFrame(this.loop)
  }
  Raindrops.prototype.start = function () {
    if (this.running) return
    this.running = true
    this.loop()
  }
  Raindrops.prototype.stop = function () {
    this.running = false
  }
  function init() {
    if (!isHome()) return
    if (isMobile()) return
    if (document.getElementById('raindrops-canvas')) return
    var c = createCanvas()
    var r = new Raindrops(c)
    r.start()
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init)
  } else {
    init()
  }
  document.addEventListener('pjax:complete', init)
})()

