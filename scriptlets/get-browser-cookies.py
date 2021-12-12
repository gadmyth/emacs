import sys
import browser_cookie3


def main():
    """Print cookies."""
    cookiejar = None
    domain = sys.argv[1]
    cookie_keys = sys.argv[2].split(":")
    if not cookiejar:
        try:
            cookiejar = browser_cookie3.chrome(domain_name=domain)
        except Exception:
            print("get cookie from Chrome failed", file=sys.stderr)

    if not cookiejar:
        try:
            cookiejar = browser_cookie3.firefox(domain_name=domain)
        except Exception:
            print("get cookie from Firefox failed", file=sys.stderr)
            return

    for cookie in cookiejar:
        print("browser cookie name:", cookie.name)
        
    cookies = list(filter(lambda c: c.name in cookie_keys, cookiejar))

    #print("cookies length: %d, cookie_keys length: %d" % (len(cookies), len(cookie_keys)))
    
    if len(cookies) < len(cookie_keys):
        print("get cookie failed, make sure you have Chrome or Firefox installed and login with one of them at least once.")
        return

    for c in cookies:
        print("cookie pair:%s:%s" % (c.name, c.value))


if __name__ == "__main__":
    main()
