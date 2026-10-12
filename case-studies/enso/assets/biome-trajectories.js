'use strict';
(async () => {
 const hosts=[...document.querySelectorAll('[data-biome-chart]')];
 const palette=['#70b7ef','#e7ad6e','#b0c879','#dd8cc4','#8ed3c7','#f1d474','#b0a2eb','#ed9393','#68c5de','#c6b394','#b8d8ef','#d9a6f2','#e8b9a3','#93cd91','#e8ceba','#d1d26f','#95aaa8'];
 const esc=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
 const number=v=>new Intl.NumberFormat('en-US',{maximumFractionDigits:2}).format(v);
 try {
 const response=await fetch('assets/biome-trajectories.json');if(!response.ok)throw Error('Data unavailable');const data=await response.json();
 hosts.forEach(host=>{
  const metric=host.dataset.biomeChart,carbon=metric==='co2',label=carbon?'Change in plant production (million tCO₂e / year)':'NPP change (% of expected NPP)',value=r=>carbon?r.co2/1e6:r.npp;
  let isolated=null;
  host.innerHTML=`<div class="trajectory-controls"><label>Show biome <select aria-label="${carbon?'Carbon production':'NPP'} biome"><option value="all">All 17 biomes</option>${data.map((d,i)=>`<option value="${i}">${esc(d.biome)}</option>`).join('')}</select></label><button type="button" class="trajectory-reset">Show all biomes</button></div><p class="trajectory-help">Hover over a line for annual estimates. Click a line or biome name to isolate it; click again or choose “Show all biomes” to reset. On a touch screen, tap a point for its value.</p><p class="trajectory-state" aria-live="polite"></p><div class="trajectory-plot"></div><div class="trajectory-readout" role="status" aria-live="polite">Hover or focus a point to inspect its estimate.</div><div class="trajectory-legend" aria-label="Select a biome">${data.map((d,i)=>`<button type="button" data-biome="${i}" aria-pressed="false"><span style="background:${palette[i]}"></span>${esc(d.biome)}</button>`).join('')}</div><details class="trajectory-table"><summary>View annual estimates as a table</summary><div></div></details>`;
  const select=host.querySelector('select'),plot=host.querySelector('.trajectory-plot'),readout=host.querySelector('.trajectory-readout');
  function inspect(i,j){const d=data[i],r=d.values[j];readout.innerHTML=`<strong style="color:${palette[i]}">${esc(d.biome)}</strong> · ${r.year} · <strong>${number(carbon?r.co2:r.npp)} ${carbon?'tCO₂e/year':'%'}</strong>${carbon?' · CO₂-equivalent mass in plant production':` · 68% model/weather range: ${number(r.npp_low)}% to ${number(r.npp_high)}%`}`;plot.querySelectorAll('.biome-series').forEach(g=>{g.style.opacity=+g.dataset.series===i?'1':'.18'});}
  function restore(){plot.querySelectorAll('.biome-series').forEach(g=>g.style.opacity='1');}
  function isolate(i){isolated=isolated===i?null:i;select.value=isolated===null?'all':String(isolated);render();}
  function render(){
   const visible=data.map((d,i)=>({d,i})).filter(x=>isolated===null||x.i===isolated),values=visible.flatMap(({d})=>d.values.flatMap(r=>!carbon&&isolated!==null?[r.npp,r.npp_low,r.npp_high]:[value(r)]));
   let lo=Math.min(0,...values),hi=Math.max(0,...values),pad=(hi-lo)*.12||1;lo-=pad;hi+=pad;
   const W=960,H=450,left=92,right=30,top=60,bottom=60,x=year=>left+(year-2026)/5*(W-left-right),y=v=>H-bottom-(v-lo)/(hi-lo)*(H-top-bottom);
   let s=`<svg viewBox="0 0 ${W} ${H}" aria-label="${esc(label)} by biome from 2026 to 2031"><text class="axis-title" x="${left}" y="23">${label}</text><rect x="${x(2027.5)}" y="${top}" width="${x(2031)-x(2027.5)}" height="${H-top-bottom}" fill="#a9b88a" opacity=".055"/><text x="${x(2028)}" y="46" class="scenario-label">2028–2031: assumed normal weather; lagged response</text>`;
   for(let j=0;j<=4;j++){const v=lo+(hi-lo)*j/4;s+=`<line x1="${left}" x2="${W-right}" y1="${y(v)}" y2="${y(v)}" class="grid-line"/><text x="${left-12}" y="${y(v)+4}" text-anchor="end">${number(v)}</text>`;}
   s+=`<line x1="${left}" x2="${W-right}" y1="${y(0)}" y2="${y(0)}" stroke="#b7b8ad" stroke-dasharray="5 5"/>`;
   for(let yr=2026;yr<=2031;yr++)s+=`<text x="${x(yr)}" y="${H-bottom+28}" text-anchor="middle">${yr}</text>`;
   visible.forEach(({d,i})=>{
    const points=d.values.map(r=>`${x(r.year)},${y(value(r))}`).join(' ');
    s+=`<g class="biome-series" data-series="${i}">`;
    if(!carbon&&isolated!==null)s+=`<polygon points="${d.values.map(r=>`${x(r.year)},${y(r.npp_low)}`).concat([...d.values].reverse().map(r=>`${x(r.year)},${y(r.npp_high)}`)).join(' ')}" fill="${palette[i]}" opacity=".16"/>`;
    s+=`<polyline points="${points}" stroke="${palette[i]}" stroke-width="2.5" fill="none" pointer-events="none"/><polyline class="line-hit" data-line="${i}" points="${points}" stroke="transparent" stroke-width="13" fill="none" tabindex="0" role="button" aria-label="Isolate ${esc(d.biome)}"/>`;
    d.values.forEach((r,j)=>{s+=`<circle cx="${x(r.year)}" cy="${y(value(r))}" r="4" fill="${palette[i]}" pointer-events="none"/><circle class="point-hit" data-point="${i},${j}" cx="${x(r.year)}" cy="${y(value(r))}" r="9" fill="transparent" tabindex="0" role="button" aria-label="${esc(d.biome)}, ${r.year}, ${number(carbon?r.co2:r.npp)} ${carbon?'tonnes CO2 equivalent per year':'percent'}"/>`;});s+='</g>';
   });plot.innerHTML=s+'</svg>';
   host.querySelector('.trajectory-state').textContent=isolated===null?'All 17 biomes · shared vertical scale':data[isolated].biome+' · vertical scale adjusted to this biome'+(!carbon?' · shaded band: 68% model/weather range':'');
   host.querySelectorAll('[data-biome]').forEach(b=>b.setAttribute('aria-pressed',String(+b.dataset.biome===isolated)));
   plot.querySelectorAll('[data-line]').forEach(el=>{const i=+el.dataset.line;el.onpointermove=e=>{const svg=plot.querySelector('svg'),pt=svg.createSVGPoint();pt.x=e.clientX;pt.y=e.clientY;const p=pt.matrixTransform(svg.getScreenCTM().inverse());inspect(i,Math.min(5,Math.max(0,Math.round((p.x-left)/(W-left-right)*5))));};el.onpointerleave=restore;el.onclick=()=>isolate(i);el.onfocus=()=>inspect(i,0);el.onblur=restore;el.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();isolate(i);}};});
   plot.querySelectorAll('[data-point]').forEach(el=>{const [i,j]=el.dataset.point.split(',').map(Number);el.onpointerenter=()=>inspect(i,j);el.onpointerleave=restore;el.onfocus=()=>inspect(i,j);el.onblur=restore;el.onclick=()=>inspect(i,j);});
   host.querySelector('.trajectory-table div').innerHTML=`<table><thead><tr><th>Biome</th>${[2026,2027,2028,2029,2030,2031].map(y=>`<th>${y}</th>`).join('')}</tr></thead><tbody>${visible.map(({d})=>`<tr><th scope="row">${esc(d.biome)}</th>${d.values.map(r=>`<td>${number(value(r))}</td>`).join('')}</tr>`).join('')}</tbody></table><p>Units: ${carbon?'million tCO₂e/year in plant production':'% of expected NPP under reference weather'}.</p>`;
  }
  select.onchange=()=>{isolated=select.value==='all'?null:+select.value;render();};host.querySelector('.trajectory-reset').onclick=()=>{isolated=null;select.value='all';render();readout.textContent='Hover or focus a point to inspect its estimate.';};host.querySelectorAll('[data-biome]').forEach(b=>b.onclick=()=>isolate(+b.dataset.biome));render();
 });
 }catch(e){hosts.forEach(h=>{h.innerHTML='<p>The interactive chart could not load. Please reload, or use the original figure below.</p>';});}
})();
