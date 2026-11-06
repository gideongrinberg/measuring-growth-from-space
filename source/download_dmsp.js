(async () => {
  const BASE = 'https://eogdata.mines.edu/wwwdata/dmsp/v4composites_rearrange/';
  const YEARS = [1992, 2008];
  const PRODUCTS = ['stable_lights.avg_vis', 'cf_cvg'];
  const index = await fetch(BASE).then(r => r.text());
  const folders = [...new Set([...index.matchAll(/href="(F\d\d_(\d{4}))\/"/g)].filter(m => +m[2] >= YEARS[0] && +m[2] <= YEARS[1]).map(m => m[1]))];
  const jobs = [];
  for (const f of folders) {
    const listing = await fetch(BASE + f + '/').then(r => r.text());
    for (const p of PRODUCTS) {
      const re = new RegExp(`href="(F\\d{6}\\.v4[a-z]?\\.global\\.${p.replace(/\./g, '\\.')}\\.tif)"`);
      const m = listing.match(re);
      m ? jobs.push(BASE + f + '/' + m[1]) : console.warn(`No ${p} file in ${f}`);
    }
  }
  console.log(`Found ${jobs.length} files`);
  const btn = document.createElement('button');
  btn.textContent = `Download ${jobs.length} DMSP files…`;
  btn.style = 'position:fixed;top:10px;right:10px;z-index:99999;padding:12px 16px;font-size:16px';
  document.body.appendChild(btn);
  btn.onclick = async () => {
    const dir = await window.showDirectoryPicker({
      mode: 'readwrite'
    });
    for (const [i, url] of jobs.entries()) {
      const name = url.split('/').pop();
      const head = await fetch(url, {
        method: 'HEAD'
      });
      if (!head.ok) {
        console.error(`HTTP ${head.status}: ${name}`);
        continue;
      }
      const size = +head.headers.get('content-length');
      try {
        const existing = await (await dir.getFileHandle(name)).getFile();
        if (existing.size === size) {
          console.log(`skip ${name}`);
          continue;
        }
      } catch {}
      btn.textContent = `${i + 1}/${jobs.length}: ${name}`;
      const res = await fetch(url);
      if (!res.ok) {
        console.error(`HTTP ${res.status}: ${name}`);
        continue;
      }
      const fh = await dir.getFileHandle(name, {
        create: true
      });
      await res.body.pipeTo(await fh.createWritable());
      console.log(`${i + 1}/${jobs.length} ${name} (${(size / 1e6).toFixed(1)} MB)`);
    }
    btn.textContent = 'Done';
    console.log('All done');
  };
})();