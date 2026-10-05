

function weightedScore(player) {
  const w = POSITION_WEIGHTS[player.position];
  if (!w) throw new Error(`No weights defined for position: ${player.position}`);
  return player.serve * w.serve
       + player.spike * w.spike
       + player.block * w.block
       + player.defence * w.defence;
}

function computeSkillsForRoster(players) {
  // 1. Compute weighted score per player
  const scored = players.map(p => ({ player: p, score: weightedScore(p) }));

  // 2. Sort ascending by score
  scored.sort((a, b) => a.score - b.score);

  const n = scored.length;

  // 3. Assign percentile rank (0 to 1) per player
  scored.forEach((entry, index) => {
    // percentile = position in sorted list / (n - 1), handles n=1 case too
    entry.percentile = n === 1 ? 1 : index / (n - 1);
  });

  // 4. Map percentile to skills 1-5 (quintiles)
  scored.forEach(entry => {
    const p = entry.percentile;
    let skills;
    if (p < 0.2) skills = 1;
    else if (p < 0.4) skills = 2;
    else if (p < 0.6) skills = 3;
    else if (p < 0.8) skills = 4;
    else skills = 5;
    entry.player.skills = skills;
  });

  return scored.map(entry => entry.player);
}