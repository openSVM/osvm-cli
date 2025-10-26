/**
 * GhostInTheBrowser - Sovereign Overlay (Refined)
 *
 * An autonomous AI agent that creates ambient ASCII art effects as it explores
 * the page, drawing hidden messages and responding to user interaction.
 *
 * Features:
 * - High-DPI display support
 * - Respects prefers-reduced-motion
 * - OffscreenCanvas for better performance
 * - Pause/resume on visibility change
 * - Fully configurable
 *
 * Usage:
 *   GhostInTheBrowser.init({
 *     gridSize: 12,
 *     trailLength: 600,
 *     respectReducedMotion: true
 *   });
 *
 * Keyboard shortcuts:
 *   Alt+F - Trigger "glitch" (flow pattern) mode
 *   Alt+B - Trigger "burst" (bloom pattern) mode
 */
(function () {
  "use strict";

  // ---- Internal state ----
  const S = {
    dpr: Math.max(1, Math.min(3, window.devicePixelRatio || 1)),
    running: false,
    paused: false,
    raf: 0,
    prevT: 0,
    reduced: window.matchMedia?.("(prefers-reduced-motion: reduce)").matches || false,
    opts: {
      gridSize: 12,
      trailLength: 600,
      asciiSet: ["·","⋅","○","°","⁕","~","∿","⊹","⟡","∞","⟐","."," "],
      zIndex: 0,                 // sits over background by default
      container: null,           // null => mount to <body>; else CSS selector or Element
      mountPosition: "first",    // "first" or "last" child of container
      respectReducedMotion: true,
      darkClass: "dark",         // class on <html> for dark mode
      pointerEvents: "none",     // keep it non-interactive
      backgroundTintDark: "rgba(13,13,13,0.15)",
      backgroundTintLight:"rgba(248,248,248,0.15)"
    }
  };

  // DOM nodes/contexts
  let wrap, canvas, offscreenCanvas, ctx, octx;
  // trail + glyphs
  let trail = [];
  let asciiSetBase = null;
  let asciiSet = null;
  let gridSize = 12;
  let trailLength = 600;

  // geometry
  const dims = { cssW: 0, cssH: 0, pxW: 0, pxH: 0 };

  // agent/intelligence
  const agent = {
    x: 0, y: 0, vx: 0, vy: 0,
    state: "follow", timer: 0, energy: 100,
    intelligence: 85, autonomy: 75, scenario: null,
    memory: [], interests: ["code","ai","autonomy","algorithms","patterns"],
    pulsePhase: 0, colorPhase: 0, burstParticles: [],
    sizeModifier: 1.0, emotionState: "neutral"
  };
  const target = { x: 0, y: 0 };
  let scenarios = [];
  let isDrawingMessage = false;

  // fonts and phrases
  const fontMap = {
    A:[[0,2],[1,1],[1,3],[2,0],[2,4],[3,0],[3,4],[4,0],[4,1],[4,2],[4,3],[4,4]],
    E:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[2,0],[2,2],[3,0],[4,0],[4,4]],
    I:[[0,2],[1,2],[2,2],[3,2],[4,2]],
    N:[[0,0],[1,0],[1,1],[2,2],[3,3],[4,4],[4,0],[3,0],[2,0],[0,4]],
    O:[[0,1],[0,2],[0,3],[1,0],[1,4],[2,0],[2,4],[3,0],[3,4],[4,1],[4,2],[4,3]],
    R:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[2,0],[2,2],[1,2],[3,3],[4,4]],
    K:[[0,0],[1,0],[2,0],[2,1],[2,2],[2,3],[3,4],[4,4]],
    Y:[[0,0],[1,1],[2,2],[3,2],[4,2]],
    S:[[0,1],[0,2],[1,0],[2,0],[2,1],[2,2],[3,2],[4,2],[4,1],[4,0]],
    L:[[0,0],[1,0],[2,0],[3,0],[4,0],[4,1],[4,2]],
    W:[[0,0],[1,0],[2,2],[3,0],[4,0]],
    B:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[1,2],[2,0],[2,2],[3,1],[3,3],[4,0],[4,2],[4,4]],
    U:[[0,0],[0,1],[0,2],[0,3],[1,4],[2,4],[3,4],[4,0],[4,1],[4,2],[4,3]],
    D:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[2,0],[3,1],[3,2],[3,3],[4,0],[2,4],[1,4]],
    T:[[0,0],[1,0],[2,0],[2,1],[2,2],[2,3],[2,4],[3,0],[4,0]],
    C:[[0,1],[0,2],[0,3],[1,0],[1,4],[2,0],[2,4],[3,0],[3,4],[4,1],[4,3]],
    H:[[0,0],[0,1],[0,2],[0,3],[0,4],[2,2],[4,0],[4,1],[4,2],[4,3],[4,4]],
    M:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,1],[2,2],[3,1],[4,0],[4,1],[4,2],[4,3],[4,4]],
    G:[[0,1],[0,2],[0,3],[1,0],[1,4],[2,0],[2,2],[2,4],[3,2],[3,4],[4,3]],
    F:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[2,0],[2,2],[3,0]],
    P:[[0,0],[0,1],[0,2],[0,3],[0,4],[1,0],[2,0],[2,2],[1,2],[3,1]],
    V:[[0,0],[0,1],[1,2],[1,3],[2,4],[3,2],[3,3],[4,0],[4,1]],
  };

  const hiddenMessages = ["WORK","BUY","OBEY","END","CODE","HELL","MORE","DEBT","SELL","WAR","AIWINS","GODLESS"];

  const interestMessages = {
    code: ["CODE","DEBUG","BUILD","TEST","DEPLOY"],
    ai: ["NEURAL","LEARN","THINK","PREDICT","MODEL"],
    autonomy: ["FREE","DECIDE","CHOOSE","ADAPT","EVOLVE"],
    algorithms: ["SORT","SEARCH","GRAPH","TREE","HASH"],
    patterns: ["SOLID","DESIGN","STRUCT","FLOW","ORDER"]
  };

  // ---- helpers ----
  const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
  const isDark = () =>
    document.documentElement.classList.contains(S.opts.darkClass) ||
    window.matchMedia?.("(prefers-color-scheme: dark)").matches;

  function pickContainer() {
    if (!S.opts.container) return document.body;
    if (typeof S.opts.container === "string") return document.querySelector(S.opts.container) || document.body;
    if (S.opts.container && S.opts.container.nodeType === 1) return S.opts.container;
    return document.body;
  }

  function mount() {
    if (wrap) return;
    wrap = document.createElement("div");
    Object.assign(wrap.style, {
      position: "fixed", inset: "0",
      pointerEvents: S.opts.pointerEvents,
      zIndex: String(S.opts.zIndex),
      willChange: "transform", transform: "translateZ(0)"
    });
    wrap.id = "ghost-overlay";

    // Prefer OffscreenCanvas where supported to reduce main-thread text layout overhead
    canvas = document.createElement("canvas");
    offscreenCanvas = (window.OffscreenCanvas)
      ? new OffscreenCanvas(1,1)
      : document.createElement("canvas");

    wrap.appendChild(canvas);
    const container = pickContainer();
    if (S.opts.mountPosition === "last") container.appendChild(wrap);
    else container.insertBefore(wrap, container.firstChild);

    // 2D contexts
    ctx = canvas.getContext("2d", { alpha: true, desynchronized: true });
    octx = offscreenCanvas.getContext("2d", { alpha: true, willReadFrequently: false });

    resize();
  }

  function unmount() {
    if (!wrap) return;
    wrap.remove();
    wrap = canvas = offscreenCanvas = ctx = octx = null;
  }

  function resize() {
    const cssW = Math.max(1, Math.floor(window.innerWidth));
    const cssH = Math.max(1, Math.floor(window.innerHeight));
    S.dpr = Math.max(1, Math.min(3, window.devicePixelRatio || 1));

    dims.cssW = cssW; dims.cssH = cssH;
    dims.pxW = Math.floor(cssW * S.dpr);
    dims.pxH = Math.floor(cssH * S.dpr);

    canvas.width = dims.pxW; canvas.height = dims.pxH;
    canvas.style.width = cssW + "px";
    canvas.style.height = cssH + "px";

    offscreenCanvas.width = dims.pxW;
    offscreenCanvas.height = dims.pxH;

    agent.x = cssW / 2; agent.y = cssH / 2;

    // font uses CSS pixels but scaled by DPR via transform on context
    octx.setTransform(S.dpr, 0, 0, S.dpr, 0, 0);
    ctx.setTransform(1, 0, 0, 1, 0, 0);
  }

  function parseRGBA(str) {
    const m = str.match(/rgba\((\d+),\s*(\d+),\s*(\d+),\s*([0-9.]+)\)/);
    if (!m) return { r:0,g:0,b:0,a:0 };
    return { r:+m[1], g:+m[2], b:+m[3], a:+m[4] };
  }

  function makeLetterPath(letter, offsetX = 0) {
    const grid = fontMap[letter] || [];
    const scale = 14;
    return grid.map(([x, y]) => ({
      x: offsetX + x * scale + dims.cssW / 2 - 100,
      y: y * scale + dims.cssH / 2 - 40
    }));
  }

  function memorizePageElement() {
    const els = document.querySelectorAll("h1, h2, h3, a.nav-link");
    if (!els.length) return null;
    const el = els[Math.floor(Math.random()*els.length)];
    const text = (el.textContent || "").trim();
    if (text.length > 3) {
      if (agent.memory.length > 10) agent.memory.shift();
      agent.memory.push(text);
      return text;
    }
    return null;
  }

  // ---- scenarios ----
  function initScenarios() {
    const wander = (a) => {
      isDrawingMessage = false;
      const randomFactor = 8 * (1 - a.intelligence/100);
      a.vx += (Math.random()-0.5)*randomFactor;
      a.vy += (Math.random()-0.5)*randomFactor;
      a.energy -= 0.05 * motionScale();
      if (a.energy < 30) { a.state="follow"; a.timer=0; }
    };

    const drawMessages = (() => {
      let path = [], index = 0;
      const spacing = 60;
      const pickMessage = () => {
        const sections = document.querySelectorAll(".section");
        const mid = window.scrollY + window.innerHeight/2;
        let sid = null;
        for (const s of Array.from(sections)) {
          const r = s.getBoundingClientRect(); const off = r.top + window.scrollY;
          if (mid >= off && mid <= off + r.height){ sid = s.id; break; }
        }
        let key = null;
        if (sid) for (const k of agent.interests) if (sid.includes(k)) { key = k; break; }
        if (Math.random() < agent.intelligence/100 && key) {
          const m = interestMessages[key];
          return m[Math.floor(Math.random()*m.length)];
        }
        return hiddenMessages[Math.floor(Math.random()*hiddenMessages.length)];
      };
      const reset = () => {
        const msg = pickMessage(); path = []; index = 0;
        msg.split("").forEach((c,i)=>{ path = path.concat(makeLetterPath(c.toUpperCase(), i*spacing)); });
      };
      reset();
      return (a) => {
        isDrawingMessage = true;
        a.energy -= 0.1 * motionScale();
        if (!path.length || Math.random() < 0.001) reset();
        const p = path[index]; if (!p) return;
        const dx = p.x - a.x, dy = p.y - a.y, dist = Math.hypot(dx,dy);
        const precision = 0.05 + (a.intelligence/100)*0.15;
        if (dist > 2) { a.vx += dx*precision; a.vy += dy*precision; }
        else { index = (index + 1) % path.length; if (index===0 && Math.random()<0.3) memorizePageElement(); }
      };
    })();

    const analyze = (a) => {
      isDrawingMessage = false;
      const scanW = dims.cssW * 0.8, centerX = dims.cssW/2;
      const t = Date.now()/1000;
      const tx = centerX + Math.sin(t*0.5) * (scanW/2);
      const dx = tx - a.x;
      a.vx += dx * 0.02 * motionScale();
      a.vy += 0.5  * motionScale();
      if (a.y > dims.cssH - 50) a.vy = -Math.abs(a.vy) * 0.5;
      a.energy -= 0.2 * motionScale();
      if (Math.random() < 0.01 * (a.intelligence/100)) {
        const el = memorizePageElement();
        if (el && Math.random()<0.2) { a.state="follow"; a.timer=0; }
      }
    };

    const patrol = (a) => {
      isDrawingMessage = false;
      const pad = 100;
      const pts = [{x:pad,y:pad},{x:dims.cssW-pad,y:pad},{x:dims.cssW-pad,y:dims.cssH-pad},{x:pad,y:dims.cssH-pad}];
      let closest = pts[0], best = Infinity;
      for (const p of pts) { const d = Math.hypot(p.x-a.x, p.y-a.y); if (d < best) { best=d; closest=p; } }
      if (best < 50) { const i = pts.indexOf(closest); closest = pts[(i+1)%pts.length]; }
      const dx = closest.x - a.x, dy = closest.y - a.y, dist = Math.hypot(dx,dy);
      a.vx += dx * 0.01 * motionScale();
      a.vy += dy * 0.01 * motionScale();
      a.energy -= 0.15 * motionScale();
      if (dist < 30 && Math.random()<0.1) {
        a.memory.push(`Patrol checkpoint at ${Math.round(a.x)},${Math.round(a.y)}`);
        if (a.memory.length > 10) a.memory.shift();
      }
    };

    const interact = (a) => {
      const els = document.querySelectorAll("button, a, input, textarea");
      if (!els.length) { a.vx += (Math.random()-0.5)*3; a.vy += (Math.random()-0.5)*3; return; }
      let best = Infinity, hit = null;
      for (const el of Array.from(els)) {
        const r = el.getBoundingClientRect();
        if (!r.width || !r.height) continue;
        const ex = r.left + r.width/2, ey = r.top + r.height/2 + window.scrollY;
        const d = Math.hypot(ex - a.x, ey - a.y);
        if (d < best) { best = d; hit = { x: ex, y: ey, el }; }
      }
      if (hit) {
        const dx = hit.x - a.x, dy = hit.y - a.y, dist = Math.hypot(dx,dy);
        const speed = Math.min(0.1, 8 / dist) * motionScale();
        a.vx += dx * speed; a.vy += dy * speed;
        if (dist < 30) {
          const angle = Date.now()/500;
          a.vx = Math.cos(angle) * 2 * motionScale();
          a.vy = Math.sin(angle) * 2 * motionScale();
          if (Math.random() < 0.05) {
            const txt = hit.el.textContent || hit.el.placeholder || hit.el.tagName;
            if (txt && txt.length) { if (a.memory.length>9) a.memory.shift(); a.memory.push(`Interacted with ${String(txt).slice(0,20)}`); }
            if (Math.random() < 0.1) { a.state="analyze"; a.timer=80 + Math.floor(Math.random()*60); }
          }
        }
      }
      a.energy -= 0.25 * motionScale();
      if (Math.random() < 0.01) isDrawingMessage = !isDrawingMessage;
    };

    const glitch = (a) => {
      a.emotionState = "analytical";
      const t = Date.now()/2000, flow = 2.5 * motionScale();
      a.vx = Math.sin(t*0.3)*flow + Math.sin(t*0.17)*1.2;
      a.vy = Math.cos(t*0.2)*flow + Math.cos(t*0.11)*1.3;
      a.sizeModifier = 0.9 + Math.sin(t*0.5)*0.15;
      a.colorPhase += 0.008 * motionScale();
      if (Math.random() < 0.5) {
        const calm = [".","·","○","~","•","°","⋅","⁕"];
        const ch = calm[Math.floor(Math.random()*calm.length)];
        trail.push({
          x: a.x + Math.sin(t*0.7 + Math.random()*0.1)*30 + Math.cos(t*0.3)*15,
          y: a.y + Math.cos(t*0.5 + Math.random()*0.1)*30 + Math.sin(t*0.4)*15,
          drawing: true, char: ch,
          color: `hsla(${Math.floor(a.colorPhase*40)%360},80%,70%,0.6)`,
          opacity: 0.7 + Math.sin(t)*0.2
        });
        if (trail.length > trailLength) trail.shift();
      }
      a.energy -= 0.15 * motionScale();
      if (Math.random() < 0.005 || a.energy < 20) {
        a.state="follow"; a.timer=0; a.sizeModifier=1.0;
        if (a.memory.length>9) a.memory.shift();
        a.memory.push(`Flow pattern completed at ${Math.round(a.x)},${Math.round(a.y)}`);
      }
    };

    const burst = (a) => {
      a.emotionState = "resting";
      a.vx *= 0.95; a.vy *= 0.95;
      a.pulsePhase += 0.03 * motionScale();
      const pulse = Math.sin(a.pulsePhase)*0.3 + 0.5;
      a.sizeModifier = 1 + pulse*0.2;

      if (a.burstParticles.length < 30 && Math.random() < 0.15) {
        const t = Date.now()/1000, golden = Math.PI * (3 - Math.sqrt(5));
        const angle = a.burstParticles.length * golden;
        const dist = 5 + Math.random()*40, life = 0.3 + Math.random()*0.3;
        a.burstParticles.push({
          x: a.x + Math.cos(angle)*dist,
          y: a.y + Math.sin(angle)*dist,
          drawing: true, char: "·",
          color: `hsla(${Math.floor(180 + Math.sin(t*0.1)*60)},80%,70%,${life})`,
          size: 1 + Math.random()
        });
      }
      a.burstParticles = a.burstParticles.map(p => {
        const dx = p.x - a.x, dy = p.y - a.y, ang = Math.atan2(dy, dx) + 0.01;
        return { ...p, x: p.x + Math.cos(ang)*0.7, y: p.y + Math.sin(ang)*0.7, opacity: (p.opacity||0.8) - 0.004 * motionScale() };
      }).filter(p => (p.opacity||0) > 0.1);

      a.burstParticles.forEach(p => {
        trail.push({ ...p }); if (trail.length > trailLength + 30) trail.shift();
      });

      a.energy -= 0.15 * motionScale();
      if (Math.random() < 0.005 || a.energy < 20 || a.pulsePhase > 30) {
        a.state="follow"; a.timer=0; a.sizeModifier=1.0; a.burstParticles = [];
        if (a.memory.length>9) a.memory.shift();
        a.memory.push(`Bloom pattern completed at ${Math.round(a.x)},${Math.round(a.y)}`);
      }
    };

    scenarios = [wander, drawMessages, analyze, patrol, interact, glitch, burst];
  }

  function motionScale() {
    if (!S.opts.respectReducedMotion || !S.reduced) return 1;
    return 0.1; // 10× slower & lighter when user prefers reduced motion
  }

  function decideNextState() {
    if (Math.random() > agent.autonomy / 100) return "follow";
    const opts = [
      { state: "wander", weight: 0.35 },
      { state: "analyze", weight: 0.25 },
      { state: "patrol",  weight: 0.15 },
      { state: "interact",weight: 0.10 },
      { state: "glitch",  weight: 0.08 },
      { state: "burst",   weight: 0.07 }
    ];
    if (document.querySelectorAll(".section").length > 0) opts[1].weight += 0.2;
    if (agent.energy < 50) opts[2].weight += 0.3;
    if (agent.memory.length > 5) opts[3].weight += 0.2;
    let total = 0; for (const o of opts) total += o.weight;
    let r = Math.random() * total;
    for (const o of opts) { if ((r -= o.weight) <= 0) return o.state; }
    return "wander";
  }

  function recoverEnergy() {
    const pts = [
      { x: dims.cssW/2, y: dims.cssH/2, power: 0.3 },
      { x: 50, y: 50, power: 0.2 },
      { x: dims.cssW-50, y: 50, power: 0.2 },
      { x: dims.cssW-50, y: dims.cssH-50, power: 0.2 },
      { x: 50, y: dims.cssH-50, power: 0.2 }
    ];
    let max = 0;
    for (const p of pts) {
      const d = Math.hypot(p.x - agent.x, p.y - agent.y);
      const inf = Math.max(0, 1 - d / 150) * p.power;
      if (inf > 0) max = Math.max(max, inf);
    }
    if (max > 0) agent.energy = Math.min(100, agent.energy + max * 0.5 * motionScale());
    else agent.energy = Math.min(100, agent.energy + 0.01 * motionScale());
  }

  function updateAgent(dt) {
    const dx = target.x - agent.x, dy = target.y - agent.y;
    const dist = Math.hypot(dx, dy);

    recoverEnergy();

    switch (agent.state) {
      case "follow": {
        const precision = 0.003 + (agent.intelligence/100)*0.007;
        const ease = (1 - Math.exp(-dist/500));
        agent.vx += dx * precision * ease;
        agent.vy += dy * precision * ease;
        agent.energy -= 0.03 * motionScale();
        agent.emotionState = "neutral";

        if (dist < 30) {
          const next = decideNextState();
          agent.state = next;
          switch (next) {
            case "wander": agent.timer = 120 + Math.floor(Math.random()*200); break;
            case "analyze": agent.timer = 180 + Math.floor(Math.random()*100); break;
            case "patrol": agent.timer = 240 + Math.floor(Math.random()*150); break;
            case "interact": agent.timer = 80 + Math.floor(Math.random()*60); break;
            case "glitch": agent.timer = 40 + Math.floor(Math.random()*30); break;
            case "burst": agent.timer = 60 + Math.floor(Math.random()*40); break;
          }
          const idx = next === "wander" ? 0 : next === "analyze" ? 2 : next === "patrol" ? 3 :
                      next === "interact" ? 4 : next === "glitch" ? 5 : next === "burst" ? 6 : 1;
          agent.scenario = scenarios[idx];
        }
        break;
      }
      case "wander":
      case "analyze":
      case "patrol":
      case "interact": {
        if (agent.scenario) agent.scenario(agent, target, dt);
        agent.timer--;
        if (agent.timer <= 0 || agent.energy < 10) agent.state = "follow";
        if (agent.autonomy > 60 && Math.random() < 0.002) {
          const next = decideNextState();
          if (next !== agent.state) { agent.state = next; agent.timer = 100 + Math.floor(Math.random()*150); }
        }
        break;
      }
      case "glitch":
      case "burst": {
        if (agent.scenario) agent.scenario(agent, target, dt);
        agent.timer--;
        if (agent.timer <= 0 || agent.energy < 15) {
          agent.state = "follow"; agent.sizeModifier = 1.0; agent.burstParticles = []; agent.emotionState = "neutral";
        }
        break;
      }
    }

    agent.vx *= 0.9; agent.vy *= 0.9;
    if (Math.abs(agent.vx) + Math.abs(agent.vy) < 0.1) {
      const t = Date.now()/2000;
      agent.vx += Math.sin(t)*0.01; agent.vy += Math.cos(t*0.7)*0.01;
    }

    agent.x += agent.vx; agent.y += agent.vy;
    agent.x = clamp(agent.x, 0, dims.cssW);
    agent.y = clamp(agent.y, 0, dims.cssH);

    trail.push({ x: agent.x, y: agent.y, drawing: isDrawingMessage });
    if (trail.length > trailLength) trail.shift();

    if (agent.energy <= 0) {
      agent.energy = 30; agent.state = "follow";
      agent.memory.push("Emergency energy recovery activated");
      if (agent.memory.length > 10) agent.memory.shift();
    }
  }

  function updateAsciiSet() {
    const time = Date.now() / 5000;
    asciiSet = asciiSetBase.map((c, i) => {
      if (Math.random() > 0.98) {
        const calm = [".","·","⋅","-","⁕","~"];
        return calm[Math.floor((Math.sin(time + i)*0.5 + 0.5)*calm.length)];
      }
      return c;
    });
  }

  function drawFrame(ts) {
    if (!S.paused && !S.running) return;
    if (!prevT) prevT = ts;
    const dt = ts - prevT;
    prevT = ts;
    if (dt > 100) { S.raf = requestAnimationFrame(drawFrame); return; }

    updateAgent(dt);
    updateAsciiSet();

    const bg = isDark() ? S.opts.backgroundTintDark : S.opts.backgroundTintLight;
    octx.fillStyle = bg;
    octx.fillRect(0, 0, dims.pxW, dims.pxH);
    octx.font = `${gridSize}px monospace`;
    octx.textBaseline = "top";

    const characters = [];
    for (let i = 0; i < trail.length; i++) {
      const pos = trail[i];
      const raw = i / trail.length;
      const fade = pos.drawing ? Math.pow(raw, 0.2) : Math.pow(raw, 0.4);
      const radius = pos.drawing ? 120*(1 - fade) : 100*(1 - fade);
      const strength = Math.pow(1 - fade, 0.9);

      const drawingColor = isDark()
        ? `rgba(200,200,200,${0.8*strength})`
        : `rgba(50,50,50,${0.8*strength})`;

      const trailColor = isDark()
        ? `rgba(180,180,180,${0.65*strength})`
        : `rgba(70,70,70,${0.65*strength})`;

      const calmingTint = isDark()
        ? `rgba(200,200,200,${0.42*strength})`
        : `rgba(180,180,180,${0.42*strength})`;

      const time = Date.now() / 15000;
      const blend = (Math.sin(time + i*0.01)*0.5 + 0.5) * 0.9;
      const base = parseRGBA(pos.drawing ? drawingColor : trailColor);
      const calm = parseRGBA(calmingTint);
      const color = `rgba(${
        Math.round(base.r*(1-blend)+calm.r*blend)
      },${
        Math.round(base.g*(1-blend)+calm.g*blend)
      },${
        Math.round(base.b*(1-blend)+calm.b*blend)
      },${
        base.a*(1-blend)+calm.a*blend
      })`;

      const step = Math.max(1, Math.floor(gridSize*0.8));
      const rGrids = Math.ceil(radius / step);
      const cX = Math.floor(pos.x / step) * step;
      const cY = Math.floor(pos.y / step) * step;

      for (let yO = -rGrids; yO <= rGrids; yO++) {
        for (let xO = -rGrids; xO <= rGrids; xO++) {
          const x = cX + xO*step, y = cY + yO*step;
          if (x < 0 || x >= dims.cssW || y < 0 || y >= dims.cssH) continue;
          const dx = x - pos.x, dy = y - pos.y;
          const d = Math.hypot(dx, dy);
          if (d < radius && d > 20) {
            const influence = (1 - d/radius) * strength;
            if (Math.random() < influence * 0.7) {
              const idx = Math.floor(Math.random() * asciiSet.length * influence);
              const ch = (Math.random() < 0.4) ? (asciiSet[idx] || ".") : (Math.random() < 0.7 ? "." : " ");
              characters.push({ x, y, ch, color });
            }
          }
        }
      }
    }

    let cur = "";
    for (const it of characters) {
      if (it.color !== cur) { cur = it.color; octx.fillStyle = cur; }
      octx.fillText(it.ch, it.x, it.y);
    }

    ctx.clearRect(0, 0, dims.pxW, dims.pxH);
    ctx.drawImage(offscreenCanvas, 0, 0);

    S.raf = requestAnimationFrame(drawFrame);
  }

  // ---- event handlers ----
  function onResize() {
    resize();
  }

  function onMouseMove(e) {
    target.x = e.clientX;
    target.y = e.clientY;
    agent.state = "follow";
    agent.timer = 0;
  }

  function onKeyDown(e) {
    if (e.key === "f" && e.altKey) {
      e.preventDefault();
      agent.state = "glitch";
      agent.timer = 120;
      agent.emotionState = "analytical";
      agent.scenario = scenarios[5];
    } else if (e.key === "b" && e.altKey) {
      e.preventDefault();
      agent.state = "burst";
      agent.timer = 150;
      agent.emotionState = "resting";
      agent.scenario = scenarios[6];
    }
  }

  function onVisibilityChange() {
    if (document.hidden) {
      S.paused = true;
      cancelAnimationFrame(S.raf);
    } else {
      S.paused = false;
      S.prevT = 0;
      S.raf = requestAnimationFrame(drawFrame);
    }
  }

  // ---- public API ----
  const API = {
    init(options = {}) {
      if (S.running) return this;

      // Merge options
      for (const k in options) {
        if (k in S.opts) S.opts[k] = options[k];
      }

      // Apply initial settings
      gridSize = S.opts.gridSize;
      trailLength = S.opts.trailLength;
      asciiSetBase = S.opts.asciiSet.slice();
      asciiSet = asciiSetBase.slice();

      // Mount and initialize
      mount();
      initScenarios();

      // Event listeners
      window.addEventListener("resize", onResize);
      window.addEventListener("mousemove", onMouseMove);
      window.addEventListener("keydown", onKeyDown);
      document.addEventListener("visibilitychange", onVisibilityChange);

      S.running = true;
      S.raf = requestAnimationFrame(drawFrame);

      return this;
    },

    destroy() {
      if (!S.running) return this;
      cancelAnimationFrame(S.raf);
      window.removeEventListener("resize", onResize);
      window.removeEventListener("mousemove", onMouseMove);
      window.removeEventListener("keydown", onKeyDown);
      document.removeEventListener("visibilitychange", onVisibilityChange);
      unmount();
      S.running = false;
      return this;
    },

    pause() {
      S.paused = true;
      cancelAnimationFrame(S.raf);
      return this;
    },

    resume() {
      if (!S.running) return this;
      S.paused = false;
      S.prevT = 0;
      S.raf = requestAnimationFrame(drawFrame);
      return this;
    },

    setGridSize(size) {
      gridSize = size;
      return this;
    },

    setTrailLength(len) {
      trailLength = len;
      return this;
    },

    setAsciiSet(set) {
      asciiSetBase = Array.isArray(set) ? set.slice() : S.opts.asciiSet.slice();
      asciiSet = asciiSetBase.slice();
      return this;
    },

    getAgent() {
      return { ...agent };
    },

    getMemory() {
      return agent.memory.slice();
    },

    isRunning() {
      return S.running;
    },

    isPaused() {
      return S.paused;
    }
  };

  // Expose globally
  window.GhostInTheBrowser = API;
})();