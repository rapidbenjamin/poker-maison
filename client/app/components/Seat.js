import React from 'react';

const Seat = ({ playerName, chips, isTurnToAct, position }) => (
  <div className={`seat-${position}-container`}>
    <div className={`seat-${position}`}>
      <h2>{playerName || 'Seat'}</h2>
      {playerName ? <h2>{chips}</h2> : ''}
    </div>
  </div>);

export default Seat;