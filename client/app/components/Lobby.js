import React from 'react';

const tableBody = lobby =>
  <tbody >
    {lobby.map(({
      _tableName,
      _playerCount,
      _minBuyInChips,
      _maxBuyInChips,
      _maxPlayers,
      _waitlistCount,
      _bigBlind }) =>
      <tr key={_tableName}>
        <td>{_tableName}</td>
        <td>{`${_playerCount} / ${_maxPlayers}`}</td>
        <td>{_waitlistCount}</td>
        <td>{_minBuyInChips}</td>
        <td>{_maxBuyInChips}</td>
        <td>{_bigBlind}</td>
      </tr>)}
  </tbody >

const Lobby = ({ lobby }) =>
  <table className="table">
    <thead>
      <tr>
        <th>Name</th>
        <th>Players</th>
        <th>Waitlist</th>
        <th>Min BuyIn</th>
        <th>Max BuyIn</th>
        <th>Big Blind</th>
      </tr>
    </thead>
    {tableBody(lobby)}
  </table>

export default Lobby;
