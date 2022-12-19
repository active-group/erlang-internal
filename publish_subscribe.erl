-module(publish_subscribe).

-behavior(gen_server).

% Gefragt ist ein "Message-Bus" mit zwei Operationen:
% - Prozesse können sich beim Bus registrieren

%   an die anderen Prozesse verteilt werden.
% Bonus: Sterbende Prozesse werden automatisch deregistriert.
