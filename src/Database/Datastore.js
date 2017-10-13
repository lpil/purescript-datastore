const datastore = require("@google-cloud/datastore");

exports.makeClient = function(creds) {
  return datastore(creds);
};

exports.makeKindKey = function(client) {
  return function(kindName) {
    return client.key(kindName);
  };
};

exports.makeKey = function(client) {
  return function(kindName) {
    return function(ids) {
      const keyArg = [kindName].concat(ids);
      return client.key(keyArg);
    };
  };
};

exports._get = function(client) {
  return function(key) {
    return function(onError, onSuccess) {
      client.get(key, function(err, res) {
        if (err) {
          onError(err);
        } else {
          onSuccess(res);
        }
      });
      return function(cancelError, onCancelerError, onCancelerSuccess) {
        // TODO: This cannot be cancelled. Should we error here instead?
        onCancelerSuccess();
      };
    };
  };
};

exports._save = function(client) {
  return function(key) {
    return function(data) {
      return function(onError, onSuccess) {
        const payload = {
          key: key,
          data: data
        };
        client.save(payload, function(err) {
          if (err) {
            onError(err);
          } else {
            onSuccess();
          }
        });
        return function(cancelError, onCancelerError, onCancelerSuccess) {
          // TODO: This cannot be cancelled. Should we error here instead?
          onCancelerSuccess();
        };
      };
    };
  };
};
