import { useState, useEffect } from "react";
import { MessageSquare, BarChart3 } from "lucide-react";
import { ChatContainer } from "./components/ChatContainer";
import { StatsPage } from "./components/StatsPage";
import { ThemeProvider } from "./contexts/ThemeContext";
import { ThemeSelector } from "./components/ThemeSelector";

type Page = "chat" | "stats";

const PAGE_STORAGE_KEY = "foodlog-current-page";

function App() {
  const [currentPage, setCurrentPage] = useState<Page>(() => {
    // Load saved page preference from localStorage
    const saved = localStorage.getItem(PAGE_STORAGE_KEY);
    return (saved === "stats" ? "stats" : "chat") as Page;
  });
  const [statsRefreshKey, setStatsRefreshKey] = useState(0);

  // Save current page to localStorage whenever it changes
  useEffect(() => {
    localStorage.setItem(PAGE_STORAGE_KEY, currentPage);
  }, [currentPage]);

  const handleMealConfirmed = () => {
    // Trigger stats page refresh by updating the key
    setStatsRefreshKey((prev) => prev + 1);
  };

  return (
    <ThemeProvider>
      <div className="relative h-screen flex flex-col bg-ctp-base">
        {/* Top Navigation Bar */}
        <header className="fixed top-0 left-0 right-0 bg-ctp-surface0 text-ctp-text shadow-md z-50">
          <div className="flex items-center px-4 py-3">
            {/* Theme selector on the left */}
            <div className="flex gap-2">
              <ThemeSelector />
            </div>

            {/* Title centered */}
            <h1 className="text-lg font-semibold flex-1 text-center">
              FoodLog
            </h1>

            {/* Navigation buttons on the right */}
            <div className="flex gap-2">
              <button
                onClick={() => setCurrentPage("chat")}
                className={`p-2 rounded-lg transition-colors ${
                  currentPage === "chat"
                    ? "bg-ctp-surface2"
                    : "hover:bg-ctp-surface1"
                }`}
                title="Chat"
              >
                <MessageSquare size={20} />
              </button>
              <button
                onClick={() => setCurrentPage("stats")}
                className={`p-2 rounded-lg transition-colors ${
                  currentPage === "stats"
                    ? "bg-ctp-surface2"
                    : "hover:bg-ctp-surface1"
                }`}
                title="Statistiche"
              >
                <BarChart3 size={20} />
              </button>
            </div>
          </div>
        </header>

        {/* Main content - with top padding to account for fixed header */}
        <div className="flex-1 overflow-hidden pt-[57px]">
          <div className={currentPage === "chat" ? "block h-full" : "hidden"}>
            <ChatContainer onMealConfirmed={handleMealConfirmed} />
          </div>
          <div className={currentPage === "stats" ? "block h-full" : "hidden"}>
            <StatsPage key={statsRefreshKey} />
          </div>
        </div>
      </div>
    </ThemeProvider>
  );
}

export default App;
