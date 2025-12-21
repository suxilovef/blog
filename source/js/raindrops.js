;(function () {
  function getEnabled() {
    var v = localStorage.getItem('raindrops-enabled')
    if (v === null) return true
    return v === 'true'
  }
  function setEnabled(flag) {
    localStorage.setItem('raindrops-enabled', flag ? 'true' : 'false')
  }
  function getIntensity() {
    var v = localStorage.getItem('raindrops-intensity')
    return v || 'heavy'
  }
  function setIntensity(v) {
    localStorage.setItem('raindrops-intensity', v)
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
    var iLvl = getIntensity()
    var base = iLvl === 'heavy' ? Math.floor(window.innerWidth / 5) : iLvl === 'light' ? Math.floor(window.innerWidth / 12) : Math.floor(window.innerWidth / 9)
    var minC = iLvl === 'heavy' ? 120 : iLvl === 'light' ? 40 : 60
    var count = Math.max(base, minC)
    var i = 0
    this.drops.length = 0
    for (i = 0; i < count; i++) {
      this.drops.push(this.spawn())
    }
  }
  Raindrops.prototype.spawn = function () {
    var w = window.innerWidth
    var h = window.innerHeight
    var iLvl = getIntensity()
    var vy = iLvl === 'heavy' ? 3 + Math.random() * 4.5 : iLvl === 'light' ? 1 + Math.random() * 2.5 : 1.5 + Math.random() * 3.5
    var len = iLvl === 'heavy' ? 16 + Math.random() * 28 : iLvl === 'light' ? 8 + Math.random() * 18 : 10 + Math.random() * 22
    var x = Math.random() * w
    var y = -Math.random() * h
    var opacity = iLvl === 'heavy' ? 0.5 + Math.random() * 0.3 : iLvl === 'light' ? 0.3 + Math.random() * 0.2 : 0.35 + Math.random() * 0.25
    var width = iLvl === 'heavy' ? 1.2 + Math.random() * 1.6 : iLvl === 'light' ? 0.7 + Math.random() * 1.2 : 0.8 + Math.random() * 1.6
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
      d.vy += getIntensity() === 'heavy' ? 0.25 : this.gravity
      d.vx = wind
      d.x += d.vx
      d.y += d.vy
      if (d.x > w + 20) d.x = -20
      else if (d.x < -20) d.x = w + 20
      if (d.y - d.len > h) {
        this.splashes.push({ x: d.x, y: h - 2, r: 0, alpha: 0.6 + Math.random() * 0.2, growth: getIntensity() === 'heavy' ? 1.4 + Math.random() * 0.8 : 0.9 + Math.random() * 0.6 })
        this.drops[i] = this.spawn()
      }
    }
    for (var j = this.splashes.length - 1; j >= 0; j--) {
      var s = this.splashes[j]
      s.r += s.growth
      s.alpha *= s.decay !== undefined ? s.decay : 0.92
      var mr = s.maxR !== undefined ? s.maxR : getIntensity() === 'heavy' ? 26 : 18
      if (s.alpha < 0.02 || s.r > mr) this.splashes.splice(j, 1)
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
    ctx.globalCompositeOperation = 'screen'
    for (var j = 0; j < this.splashes.length; j++) {
      var s = this.splashes[j]
      if (s.kind === 'click') {
        var lw = s.lw !== undefined ? s.lw : getIntensity() === 'heavy' ? 2.2 : 1.6
        ctx.shadowColor = 'rgba(200, 220, 255,' + Math.max(s.alpha * 0.6, 0) + ')'
        ctx.shadowBlur = 10
        ctx.strokeStyle = 'rgba(210, 225, 255,' + Math.max(s.alpha * 0.9, 0) + ')'
        ctx.lineWidth = lw
        ctx.beginPath()
        ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2)
        ctx.stroke()
        ctx.shadowBlur = 6
        ctx.strokeStyle = 'rgba(210, 225, 255,' + Math.max(s.alpha * 0.5, 0) + ')'
        ctx.lineWidth = Math.max(lw * 0.7, 1)
        ctx.beginPath()
        ctx.arc(s.x, s.y, s.r * 1.15, 0, Math.PI * 2)
        ctx.stroke()
        var rg = ctx.createRadialGradient(s.x, s.y, Math.max(s.r * 0.1, 1), s.x, s.y, s.r)
        rg.addColorStop(0, 'rgba(220, 230, 255,' + Math.max(s.alpha * 0.08, 0) + ')')
        rg.addColorStop(1, 'rgba(220, 230, 255, 0)')
        ctx.fillStyle = rg
        ctx.beginPath()
        ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2)
        ctx.fill()
        ctx.shadowBlur = 0
      } else {
        ctx.strokeStyle = 'rgba(180, 180, 200,' + s.alpha + ')'
        ctx.lineWidth = getIntensity() === 'heavy' ? 1.4 : 1
        ctx.beginPath()
        ctx.arc(s.x, s.y, s.r, 0, Math.PI * 2)
        ctx.stroke()
      }
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
    var enabled = getEnabled()
    if (isMobile()) return
    var existing = document.getElementById('raindrops-canvas')
    if (enabled) {
      if (existing) return
      var c = createCanvas()
      var r = new Raindrops(c)
      r.start()
      window.RAINDROPS_INSTANCE = r
    } else {
      if (existing) {
        try {
          existing.parentNode.removeChild(existing)
        } catch (e) {}
      }
    }
  }
  window.RAINDROPS = {
    enable: function () {
      setEnabled(true)
      init()
    },
    disable: function () {
      setEnabled(false)
      var existing = document.getElementById('raindrops-canvas')
      if (existing) {
        try {
          existing.parentNode.removeChild(existing)
        } catch (e) {}
      }
      if (window.RAINDROPS_INSTANCE) {
        try {
          window.RAINDROPS_INSTANCE.stop()
        } catch (e) {}
        window.RAINDROPS_INSTANCE = null
      }
    },
    toggle: function () {
      if (getEnabled()) this.disable()
      else this.enable()
    },
    isEnabled: function () {
      return getEnabled()
    },
    setIntensity: function (level) {
      if (!level || !/^(heavy|normal|light)$/.test(level)) return
      setIntensity(level)
      init()
    }
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init)
  } else {
    init()
  }
  document.addEventListener('pjax:complete', init)
  document.addEventListener('click', function (e) {
    if (!window.RAINDROPS_INSTANCE) return
    if (isMobile()) return
    var level = getIntensity()
    var g =
      level === 'heavy'
        ? 1.3 + Math.random() * 0.5
        : level === 'light'
        ? 0.8 + Math.random() * 0.4
        : 1 + Math.random() * 0.4
    var mr = level === 'heavy' ? 64 : level === 'light' ? 42 : 52
    var decay = level === 'heavy' ? 0.965 : level === 'light' ? 0.97 : 0.968
    var lw = level === 'heavy' ? 2.4 : level === 'light' ? 1.6 : 1.9
    window.RAINDROPS_INSTANCE.splashes.push({ x: e.clientX, y: e.clientY, r: 0, alpha: 0.95, growth: g, kind: 'click', maxR: mr, decay: decay, lw: lw })
  })
})()

