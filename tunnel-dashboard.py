#!/usr/bin/env python3
"""
Agave Validator + ngrok Tunnel Dashboard
Enhanced monitoring with Business plan features
"""

import requests
import json
import time
import sys
import os
from datetime import datetime
import subprocess

class TunnelDashboard:
    def __init__(self):
        self.ngrok_api = "http://localhost:4040/api"
        self.validator_rpc = "http://localhost:8899"
        
    def get_tunnel_info(self):
        """Get tunnel information from ngrok API"""
        try:
            response = requests.get(f"{self.ngrok_api}/tunnels", timeout=5)
            return response.json() if response.status_code == 200 else None
        except:
            return None
    
    def get_request_stats(self):
        """Get HTTP request statistics"""
        try:
            response = requests.get(f"{self.ngrok_api}/requests/http", timeout=5)
            return response.json() if response.status_code == 200 else None
        except:
            return None
    
    def test_validator_rpc(self):
        """Test validator RPC endpoint"""
        try:
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "getVersion"
            }
            response = requests.post(self.validator_rpc, json=payload, timeout=5)
            return response.status_code == 200 and "result" in response.json()
        except:
            return False
    
    def get_validator_status(self):
        """Get detailed validator status"""
        try:
            # Get version
            version_payload = {"jsonrpc": "2.0", "id": 1, "method": "getVersion"}
            version_resp = requests.post(self.validator_rpc, json=version_payload, timeout=5)
            
            # Get health
            health_payload = {"jsonrpc": "2.0", "id": 2, "method": "getHealth"}
            health_resp = requests.post(self.validator_rpc, json=health_payload, timeout=5)
            
            # Get slot
            slot_payload = {"jsonrpc": "2.0", "id": 3, "method": "getSlot"}
            slot_resp = requests.post(self.validator_rpc, json=slot_payload, timeout=5)
            
            return {
                "version": version_resp.json().get("result") if version_resp.status_code == 200 else None,
                "health": health_resp.json().get("result") if health_resp.status_code == 200 else None,
                "slot": slot_resp.json().get("result") if slot_resp.status_code == 200 else None
            }
        except:
            return {"version": None, "health": None, "slot": None}
    
    def display_header(self):
        """Display dashboard header"""
        os.system('clear' if os.name == 'posix' else 'cls')
        print("=" * 80)
        print("🚀 AGAVE VALIDATOR + NGROK TUNNEL DASHBOARD")
        print("   Enhanced with Business Plan Features & HTTP Headers")
        print("=" * 80)
        print(f"⏰ {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print()
    
    def display_tunnel_status(self, tunnel_data):
        """Display tunnel status information"""
        print("🌐 TUNNEL STATUS")
        print("-" * 40)
        
        if not tunnel_data or 'tunnels' not in tunnel_data:
            print("❌ No tunnels found or ngrok not running")
            return
        
        tunnels = tunnel_data['tunnels']
        print(f"✅ {len(tunnels)} tunnels active")
        
        for tunnel in tunnels:
            name = tunnel.get('name', 'Unknown')
            url = tunnel.get('public_url', 'N/A')
            proto = tunnel.get('proto', 'N/A')
            local_addr = tunnel.get('config', {}).get('addr', 'N/A')
            
            status = "🟢" if url.startswith('https://') else "🟡"
            print(f"  {status} {name.upper():12} | {url}")
            print(f"      └─ {proto}:{local_addr}")
        print()
    
    def display_validator_status(self, validator_status):
        """Display validator status"""
        print("⚡ VALIDATOR STATUS")
        print("-" * 40)
        
        if validator_status['version']:
            version = validator_status['version']
            print(f"✅ Agave Version: {version.get('solana-core', 'Unknown')}")
            print(f"   Feature Set: {version.get('feature-set', 'Unknown')}")
        else:
            print("❌ Validator not responding")
            
        if validator_status['health']:
            print(f"💚 Health: {validator_status['health']}")
        else:
            print("💔 Health check failed")
            
        if validator_status['slot']:
            print(f"🎯 Current Slot: {validator_status['slot']:,}")
        
        print()
    
    def display_request_stats(self, request_data):
        """Display HTTP request statistics"""
        print("📊 REQUEST ANALYTICS")
        print("-" * 40)
        
        if not request_data or 'requests' not in request_data:
            print("📭 No request data available")
            return
        
        requests_list = request_data['requests']
        if not requests_list:
            print("📭 No requests logged yet")
            return
        
        # Analyze recent requests
        recent_count = len(requests_list)
        print(f"📈 Total Requests: {recent_count}")
        
        # Count by method
        methods = {}
        locations = {}
        paths = {}
        
        for req in requests_list[-20:]:  # Last 20 requests
            method = req.get('method', 'Unknown')
            methods[method] = methods.get(method, 0) + 1
            
            # Extract location from headers
            headers = req.get('request', {}).get('headers', {})
            location = headers.get('X-Client-Loc', 'Unknown')
            if location != 'Unknown':
                locations[location] = locations.get(location, 0) + 1
            
            # Extract path
            uri = req.get('uri', '')
            if uri:
                paths[uri] = paths.get(uri, 0) + 1
        
        # Display top methods
        if methods:
            print("📊 Methods:")
            for method, count in sorted(methods.items(), key=lambda x: x[1], reverse=True)[:3]:
                print(f"   {method}: {count}")
        
        # Display top locations
        if locations:
            print("🌍 Top Locations:")
            for location, count in sorted(locations.items(), key=lambda x: x[1], reverse=True)[:3]:
                print(f"   {location}: {count}")
        
        # Display latest requests
        print("🕐 Latest Requests:")
        for req in requests_list[-3:]:
            timestamp = req.get('timestamp', 'Unknown')
            method = req.get('method', '???')
            uri = req.get('uri', '/')
            headers = req.get('request', {}).get('headers', {})
            client_ip = headers.get('X-Client-Ip', 'Unknown')
            
            if timestamp != 'Unknown':
                try:
                    ts = datetime.fromisoformat(timestamp.replace('Z', '+00:00'))
                    time_str = ts.strftime('%H:%M:%S')
                except:
                    time_str = timestamp[:8]
            else:
                time_str = 'Unknown'
                
            print(f"   [{time_str}] {method} {uri} from {client_ip}")
        
        print()
    
    def display_business_features(self):
        """Display Business plan features status"""
        print("💼 BUSINESS FEATURES ACTIVE")
        print("-" * 40)
        print("✅ Custom HTTP Headers:")
        print("   • x-is-ngrok: Request identification")
        print("   • x-client-ip: Original client IP tracking")  
        print("   • x-client-loc: Geographic location data")
        print("   • x-endpoint-id: Unique endpoint tracking")
        print("   • x-service-type: Service identification")
        print()
        print("✅ Professional Features:")
        print("   • Multiple simultaneous tunnels")
        print("   • Custom domain support")
        print("   • Advanced traffic policies")
        print("   • Enhanced monitoring & analytics")
        print()
    
    def run_dashboard(self):
        """Main dashboard loop"""
        print("Starting Agave Validator + ngrok Dashboard...")
        print("Press Ctrl+C to exit")
        
        try:
            while True:
                self.display_header()
                
                # Get data
                tunnel_data = self.get_tunnel_info()
                request_data = self.get_request_stats()
                validator_status = self.get_validator_status()
                
                # Display sections
                self.display_tunnel_status(tunnel_data)
                self.display_validator_status(validator_status)
                self.display_request_stats(request_data)
                self.display_business_features()
                
                print("🔄 Refreshing in 10 seconds... (Ctrl+C to exit)")
                time.sleep(10)
                
        except KeyboardInterrupt:
            print("\n👋 Dashboard stopped.")
            sys.exit(0)
        except Exception as e:
            print(f"\n❌ Error: {e}")
            sys.exit(1)

def main():
    """Main entry point"""
    if len(sys.argv) > 1:
        if sys.argv[1] == "--once":
            # Run once and exit
            dashboard = TunnelDashboard()
            dashboard.display_header()
            
            tunnel_data = dashboard.get_tunnel_info()
            request_data = dashboard.get_request_stats()
            validator_status = dashboard.get_validator_status()
            
            dashboard.display_tunnel_status(tunnel_data)
            dashboard.display_validator_status(validator_status)
            dashboard.display_request_stats(request_data)
            dashboard.display_business_features()
            return
    
    # Run interactive dashboard
    dashboard = TunnelDashboard()
    dashboard.run_dashboard()

if __name__ == "__main__":
    main()
