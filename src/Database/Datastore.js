const datastore = require("@google-cloud/datastore");

function makeKey(client, parts) {
  const cloned = parts.slice(0);
  return client.key(cloned);
}

exports.makeClient = function(creds) {
  return datastore(creds);
};

exports._save = function(method) {
  return function(client) {
    return function(keyArray) {
      return function(data) {
        return function(onError, onSuccess) {
          const payload = {
            key: makeKey(client, keyArray),
            data: data,
            method: method
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
};

exports._get = function(client) {
  return function(keyArray) {
    return function(onError, onSuccess) {
      const getKey = makeKey(client, keyArray);
      client.get(getKey, function(err, res) {
        if (err) {
          onError(err);
        } else {
          const key = (res || {})[client.KEY] || {};
          onSuccess({
            data: res,
            kind: key.kind,
            path: key.path,
            id: key.name || parseInt(key.id)
          });
        }
      });

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        // TODO: This cannot be cancelled. Should we error here instead?
        onCancelerSuccess();
      };
    };
  };
};

exports._delete = function(client) {
  return function(keyArray) {
    return function(onError, onSuccess) {
      const key = makeKey(client, keyArray);
      client.delete(key, function(err) {
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
