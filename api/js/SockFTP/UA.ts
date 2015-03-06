// This class is used to detect specific browser implementations (UA stands for UserAgent)

class SockFTP_UA {
	public static type: string; // will contain "o" || "ms" || "webkit" || "moz"
}

SockFTP_UA.type = ( function() {
	
	// opera seems to be the most "painted" :)
	if ( navigator && navigator.userAgent && 
         /Mozilla/.test( navigator.userAgent ) &&
         /AppleWebKit/.test( navigator.userAgent ) &&
         /Chrome/.test( navigator.userAgent ) &&
         /Safari/.test( navigator.userAgent ) &&
         / OPR\//.test( navigator.userAgent )
	) return "o"

	else {

		var o = {
			"ms"     : 0,
			"moz"    : 0,
			"webkit" : 0
		}, k, key;

		for ( k in window ) {
			key = /^(on)?(ms|moz|webkit)/.exec( k.toLowerCase() );
			if ( key ) {o[key[2]]++};
		}

		if ( o.ms > o.moz && o.ms > o.webkit ) {
			return 'ms';
		} else
		if ( o.moz > o.ms && o.moz > o.webkit ) {
			return 'moz';
		} else
		if ( o.webkit > o.moz && o.webkit > o.ms ) {
			return 'webkit';
		} else {
			return '';
		}

	}

} )();